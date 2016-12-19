// Copyright 2016 The RLS Project Developers.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// For processing the raw save-analysis data from rustc into rustw's in-memory representation.

use super::raw::{self, Format};
use super::{AnalysisHost, AnalysisLoader, PerCrateAnalysis, Span, NULL, Def, Glob, Signature, SigElement};
use util;

use span;

use std::collections::HashMap;
use std::iter::Extend;
use std::path::{Path, PathBuf};
use std::time::Instant;

// f is a function used to record the lowered crate into analysis.
pub fn lower<F, L>(raw_analysis: Vec<raw::Crate>, project_dir: PathBuf, full_docs: bool, analysis: &AnalysisHost<L>, mut f: F) -> Result<(), ()>
    where F: FnMut(&AnalysisHost<L>, PerCrateAnalysis, PathBuf) -> Result<(), ()>,
          L: AnalysisLoader
{
    let rss = util::get_resident().unwrap_or(0);
    let t_start = Instant::now();

    for c in raw_analysis.into_iter() {
        let t_start = Instant::now();

        let (per_crate, path) = CrateReader::read_crate(analysis, c, &project_dir, full_docs);

        let time = t_start.elapsed();
        info!("Lowering {} in {:.2}s", path.display(), time.as_secs() as f64 + time.subsec_nanos() as f64 / 1_000_000_000.0);
        info!("    defs:  {}", per_crate.defs.len());
        info!("    refs:  {}", per_crate.ref_spans.len());
        info!("    globs: {}", per_crate.globs.len());

        f(analysis, per_crate, path)?;
    }

    let time = t_start.elapsed();
    let rss = util::get_resident().unwrap_or(0) - rss;
    info!("Total lowering time: {:.2}s", time.as_secs() as f64 + time.subsec_nanos() as f64 / 1_000_000_000.0);
    info!("Diff in rss: {:.2}KB", rss as f64 / 1000.0);

    Ok(())
}

pub fn lower_span(raw_span: &raw::SpanData, project_dir: Option<&Path>) -> Span {
    let file_name = &raw_span.file_name;
    let file_name = if file_name.is_absolute() {
        file_name.to_owned()
    } else {
        project_dir.expect("Required project directory, but not supplied").join(file_name)
    };


    // Rustc uses 1-indexed rows and columns, the RLS uses 0-indexed.
    span::Span::new(raw_span.line_start.zero_indexed(),
                    raw_span.line_end.zero_indexed(),
                    raw_span.column_start.zero_indexed(),
                    raw_span.column_end.zero_indexed(),
                    file_name)
}

struct CrateReader {
    crate_map: Vec<u32>,
    project_dir: PathBuf,
    crate_name: String,
    full_docs: bool,
}

impl CrateReader {
    fn from_prelude(mut prelude: raw::CratePreludeData,
                    master_crate_map: &mut HashMap<String, u32>,
                    project_dir: &Path,
                    full_docs: bool)
                    -> CrateReader {
        let crate_name = prelude.crate_name.clone();
        // println!("building crate map for {}", crate_name);
        let next = master_crate_map.len() as u32;
        let mut crate_map = vec![*master_crate_map.entry(crate_name.clone()).or_insert_with(|| next)];
        // println!("  {} -> {}", crate_name, master_crate_map[&crate_name]);

        prelude.external_crates.sort_by(|a, b| a.num.cmp(&b.num));
        for c in prelude.external_crates {
            assert!(c.num == crate_map.len() as u32);
            let next = master_crate_map.len() as u32;
            crate_map.push(*master_crate_map.entry(c.name.clone()).or_insert_with(|| next));
            // println!("  {} -> {}", c.name, master_crate_map[&c.name]);
        }

        CrateReader {
            crate_map: crate_map,
            project_dir: project_dir.to_owned(),
            crate_name: crate_name,
            full_docs: full_docs,
        }
    }

    fn read_crate<L: AnalysisLoader>(
        project_analysis: &AnalysisHost<L>,
        krate: raw::Crate,
        project_dir: &Path,
        full_docs: bool)
        -> (PerCrateAnalysis, PathBuf)
    {
        let reader = CrateReader::from_prelude(krate.analysis.prelude.unwrap(),
                                               &mut project_analysis.master_crate_map.lock().unwrap(),
                                               project_dir,
                                               full_docs);

        let mut per_crate = PerCrateAnalysis::new();

        let api_crate = krate.analysis.kind == Format::JsonApi;
        if !api_crate {
            per_crate.timestamp = Some(krate.timestamp);
        }

        reader.read_defs(krate.analysis.defs, &mut per_crate, api_crate);
        reader.read_imports(krate.analysis.imports, &mut per_crate, project_analysis);
        reader.read_refs(krate.analysis.refs, &mut per_crate, project_analysis);

        (per_crate, krate.path)
    }

    fn read_imports<L: AnalysisLoader>(
        &self, imports: Vec<raw::Import>, analysis: &mut PerCrateAnalysis, project_analysis: &AnalysisHost<L>
    ) {
        for i in imports {
            let span = lower_span(&i.span, Some(&self.project_dir));
            if !i.value.is_empty() {
                // A glob import.
                let glob = Glob {
                    value: i.value,
                };
                analysis.globs.insert(span, glob);
            } else if let Some(ref ref_id) = i.ref_id {
                // Import where we know the referred def.
                let def_id = self.id_from_compiler_id(ref_id);
                if def_id != NULL && !analysis.def_id_for_span.contains_key(&span) &&
                   (project_analysis.has_def(def_id) || analysis.defs.contains_key(&def_id)) {
                    //println!("record import ref {:?} {:?} {:?} {}", i, span, ref_id, def_id);
                    analysis.def_id_for_span.insert(span.clone(), def_id);
                    analysis.ref_spans.entry(def_id).or_insert_with(|| vec![]).push(span);
                }
            }
        }
    }

    fn read_defs(&self, defs: Vec<raw::Def>, analysis: &mut PerCrateAnalysis, api_crate: bool) {
        for mut d in defs {
            let span = lower_span(&d.span, Some(&self.project_dir));
            let id = self.id_from_compiler_id(&d.id);
            if id != NULL && !analysis.defs.contains_key(&id) {
                if api_crate {
                    // TODO gross hack - take me out, and do something better in rustc
                    // TODO shit, I can't even remember why we do this - it makes no sense :-s
                    if d.kind == super::raw::DefKind::Struct {
                        d.value = String::new();
                    }
                } else {
                    let file_name = span.file.clone();
                    analysis.defs_per_file.entry(file_name).or_insert_with(|| vec![]).push(id);

                    analysis.def_id_for_span.insert(span.clone(), id);
                    analysis.def_names.entry(d.name.clone()).or_insert_with(|| vec![]).push(id);
                }
                let parent = d.parent.map(|id| self.id_from_compiler_id(&id));
                if let Some(parent) = parent {
                    analysis.children.entry(parent).or_insert_with(|| vec![]).push(id);
                }
                if let Some(children) = d.children {
                    if !children.is_empty() {
                        analysis.children.entry(id).or_insert_with(|| vec![]).extend(children.iter().map(|id| self.id_from_compiler_id(&id)));
                    }
                }

                let def = Def {
                    kind: d.kind,
                    span: span,
                    name: d.name,
                    value: d.value,
                    qualname: format!("{}{}", self.crate_name, d.qualname),
                    api_crate: api_crate,
                    parent: parent,
                    docs: match d.docs.find("\n\n") {
                        Some(index) if !self.full_docs => d.docs[..index].to_owned(),
                        _ => d.docs,
                    },
                    sig: d.sig.map(|ref s| self.lower_sig(s, Some(&self.project_dir))),
                };
                trace!("record def: {:?}", def);

                analysis.defs.insert(id, def);
            }
        }

        // We must now run a pass over the defs setting parents, because save-analysis often omits parent info
        for (parent, children) in &analysis.children {
            for c in children {
                analysis.defs.get_mut(&c).map(|def| def.parent = Some(*parent));
            }
        }
    }

    fn read_refs<L: AnalysisLoader>(&self, refs: Vec<raw::Ref>, analysis: &mut PerCrateAnalysis, project_analysis: &AnalysisHost<L>) {
        for r in refs {
            let def_id = self.id_from_compiler_id(&r.ref_id);
            let span = lower_span(&r.span, Some(&self.project_dir));
            if def_id != NULL && !analysis.def_id_for_span.contains_key(&span) &&
               (project_analysis.has_def(def_id) || analysis.defs.contains_key(&def_id)) {
                trace!("record ref {:?} {:?} {:?} {}", r.kind, span, r.ref_id, def_id);
                analysis.def_id_for_span.insert(span.clone(), def_id);
                analysis.ref_spans.entry(def_id).or_insert_with(|| vec![]).push(span);
            }
        }
    }

    fn lower_sig(&self, raw_sig: &raw::Signature, project_dir: Option<&Path>) -> Signature {
        Signature {
            span: lower_span(&raw_sig.span, project_dir),
            text: raw_sig.text.clone(),
            ident_start: raw_sig.ident_start as u32,
            ident_end: raw_sig.ident_end as u32,
            defs: raw_sig.defs.iter().map(|se| self.lower_sig_element(se)).collect(),
            refs: raw_sig.refs.iter().map(|se| self.lower_sig_element(se)).collect(),
        }
    }

    fn lower_sig_element(&self, raw_se: &raw::SigElement) -> SigElement {
        SigElement {
            id: self.id_from_compiler_id(&raw_se.id),
            start: raw_se.start,
            end: raw_se.end,
        }
    }

    fn id_from_compiler_id(&self, id: &raw::CompilerId) -> u32 {
        if id.krate == NULL || id.index == NULL {
            return NULL;
        }
        // We build an id by looking up the local crate number into a global crate number and using
        // that for the 8 high order bits, and use the least significant 24 bits of the index part
        // of the def index as the low order bits.
        let krate = self.crate_map[id.krate as usize] as u32;
        let crate_local = id.index & 0x00ffffff;
        krate << 24 | crate_local
    }
}
