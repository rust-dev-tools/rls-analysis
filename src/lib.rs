// Copyright 2016 The RLS Project Developers.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

// FIXME this whole crate all needs *lots* of optimisation.
//   could try using non-crypto hashing for perf
//      https://cdn.rawgit.com/Gankro/hash-rs/7b9cf787a830c1e52dcaf6ec37d2985c8a30bce1/index.html
//   keep a reference to the current crate and start all 'local' searches there before iterating the crate map
//   for looking up ids, we can use the crate part of the id to narrow the search, rather than searching all crates

#![feature(question_mark)]
#![feature(const_fn)]
#![feature(custom_derive, plugin)]
#![feature(type_ascription)]
#![feature(rustc_private)]

#![plugin(serde_macros)]

extern crate rustc_serialize;
extern crate serde;
extern crate serde_json;
extern crate syntax;

pub mod raw;
mod lowering;
mod listings;

pub use self::raw::Target;
use std::collections::HashMap;
use std::env;
use std::path::PathBuf;
use std::sync::Mutex;
use std::time::SystemTime;
// use syntax::codemap::Loc;

pub struct AnalysisHost {
    analysis: Mutex<Option<Analysis>>,
    path_prefix: String,
    target: Target,
    master_crate_map: Mutex<HashMap<String, u32>>,
}

pub type AResult<T> = Result<T, ()>;

macro_rules! clone_field {
    ($field: ident) => { |x| x.$field.clone() }
}

impl AnalysisHost {
    pub fn new(path_prefix: &str, target: Target) -> AnalysisHost {
        AnalysisHost {
            analysis: Mutex::new(None),
            path_prefix: path_prefix.to_owned(),
            target: target,
            master_crate_map: Mutex::new(HashMap::new()),
        }
    }

    pub fn reload(&self) -> AResult<()> {
        let timestamps = match self.analysis.lock() {
            Ok(a) => {
                match &*a {
                    &Some(ref a) => Some(a.timestamps()),
                    &None => None,
                }
            }
            Err(_) => return Err(()),
        };

        let timestamps = match timestamps {
            Some(ts) => ts,
            None => return self.hard_reload(),
        };

        let raw_analysis = raw::Analysis::read_incremental(&self.path_prefix, self.target, timestamps);

        lowering::lower(raw_analysis, self.mk_project_dir(), self, |host, per_crate, path| {
            match host.analysis.lock() {
                Ok(mut a) => {
                    a.as_mut().unwrap().update(per_crate, path);
                    Ok(())
                }
                Err(_) => Err(()),
            }
        })
    }

    // Reloads the entire project's analysis data.
    pub fn hard_reload(&self) -> AResult<()> {
        let raw_analysis = raw::Analysis::read(&self.path_prefix, self.target);

        // We're going to create a dummy AnalysisHost that we will fill with data,
        // then once we're done, we'll swap its data into self.
        let mut new_host = AnalysisHost::new(&self.path_prefix, self.target);
        new_host.analysis = Mutex::new(Some(Analysis::new()));
        lowering::lower(raw_analysis, self.mk_project_dir(), &mut new_host, |host, per_crate, path| {
            host.analysis.lock().unwrap().as_mut().unwrap().per_crate.insert(path, per_crate);
            Ok(())
        })?;

        match self.master_crate_map.lock() {
            Ok(mut mcm) => {
                *mcm = new_host.master_crate_map.into_inner().unwrap();
            }
            Err(_) => return Err(()),
        }
        match self.analysis.lock() {
            Ok(mut a) => {
                *a = Some(new_host.analysis.into_inner().unwrap().unwrap());
                Ok(())
            }
            Err(_) => Err(()),
        }
    }

    fn mk_project_dir(&self) -> String {
        format!("{}/{}", env::current_dir().unwrap().display(), self.path_prefix)
    }

    pub fn has_def(&self, id: u32) -> bool {
        match self.analysis.lock() {
            Ok(a) => a.as_ref().unwrap().has_def(id),
            _ => false,
        }
    }

    pub fn goto_def(&self, span: &Span) -> AResult<Span> {
        self.read(|a| {
            a.def_id_for_span(span)
             .and_then(|id| a.with_defs(id, clone_field!(span)))
        })
    }

    pub fn find_all_refs(&self, span: &Span) -> AResult<Vec<Span>> {
        self.read(|a| {
            a.def_id_for_span(span)
             .and_then(|id| {
                a.with_ref_spans(id, |refs| {
                    a.with_defs(id, clone_field!(span))
                     .into_iter()
                     .chain(refs.iter().cloned())
                     .collect::<Vec<_>>()
                 })
                 .or(a.with_defs(id, clone_field!(span)).map(|s| vec![s]))
             })
        })
    }

    pub fn show_type(&self, span: &Span) -> AResult<String> {
        self.read(|a| {
            a.def_id_for_span(span)
             .and_then(|id| a.with_defs(id, clone_field!(value)))
        })
    }

    pub fn docs(&self, span: &Span) -> AResult<String> {
        self.read(|a| {
            a.def_id_for_span(span)
             .and_then(|id| a.with_defs(id, clone_field!(docs)))
         })
    }

    pub fn search(&self, name: &str) -> AResult<Vec<Span>> {
        self.read(|a| {
            a.with_def_names(name, |defs| {
                defs.into_iter()
                     .flat_map(|id| {
                        a.with_ref_spans(*id, |v| v.clone()).unwrap_or(vec![]).into_iter()
                     })
                     .collect(): Vec<Span>
             })
        })
    }

    pub fn symbols(&self, file_name: &str) -> AResult<Vec<SymbolResult>> {
        self.read(|a| {
            a.with_defs_per_file(file_name, |ids| {
                ids.iter()
                   .map(|id| a.with_defs(*id, |def| SymbolResult::new(*id, def)).unwrap())
                   .collect()
            })
        })
    }

    pub fn doc_url(&self, span: &Span) -> AResult<String> {
        // e.g., https://doc.rust-lang.org/nightly/std/string/String.t.html
        self.read(|a| {
            a.def_id_for_span(span)
             .and_then(|id| a.with_defs_and_then(id, |def| AnalysisHost::mk_doc_url(def, a)))
        })
    }

    pub fn src_url(&self, span: &Span) -> AResult<String> {
        // e.g., https://github.com/rust-lang/rust/blob/master/src/libcollections/string.rs#L261-L263
        self.read(|a| {
            a.def_id_for_span(span)
             .and_then(|id| a.with_defs(id, |def| format!("{}/{}#L{}-L{}",
                                                          a.src_url_base,
                                                          def.span.file_name,
                                                          def.span.line_start,
                                                          def.span.line_end)))
        })
    }

    fn read<F, T>(&self, f: F) -> AResult<T>
        where F: FnOnce(&Analysis) -> Option<T>
    {
        match self.analysis.lock() {
            Ok(a) => {
                if let Some(ref a) = *a {
                    f(a).ok_or(())
                } else {
                    Err(())
                }
            }
            _ => Err(())
        }
    }

    fn mk_doc_url(def: &Def, analysis: &Analysis) -> Option<String> {
        if def.parent.is_none() && def.qualname.contains('<') {
            println!("mk_doc_url, bailing, found generic qualname: `{}`", def.qualname);
            return None;
        }

        // TODO bail out if not stdlibs

        match def.parent {
            Some(p) => {
                analysis.with_defs(p, |parent| {
                    let parent_qualpath = parent.qualname.replace("::", "/");
                    let ns = def.kind.name_space();
                    format!("{}/{}.t.html#{}.{}", analysis.doc_url_base, parent_qualpath, def.name, ns)                    
                })
            }
            None => {
                let qualpath = def.qualname.replace("::", "/");
                let ns = def.kind.name_space();
                Some(format!("{}/{}.{}.html", analysis.doc_url_base, qualpath, ns))
            }
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct SymbolResult {
    pub id: u32,
    pub name: String,
    pub kind: raw::DefKind,
    pub span: Span,
}

impl SymbolResult {
    fn new(id: u32, def: &Def) -> SymbolResult {
        SymbolResult {
            id: id,
            name: def.name.clone(),
            span: def.span.clone(),
            kind: def.kind.clone(),
        }
    }
}

#[derive(Debug)]
pub struct Analysis {
    per_crate: HashMap<PathBuf, PerCrateAnalysis>,

    pub doc_url_base: String,
    pub src_url_base: String,
}

#[derive(Debug)]
pub struct PerCrateAnalysis {
    // Map span to id of def (either because it is the span of the def, or of the def for the ref).
    def_id_for_span: HashMap<Span, u32>,
    defs: HashMap<u32, Def>,
    defs_per_file: HashMap<String, Vec<u32>>,
    def_names: HashMap<String, Vec<u32>>,
    ref_spans: HashMap<u32, Vec<Span>>,

    timestamp: Option<SystemTime>,
}

#[derive(Debug, Clone, Hash, Ord, PartialOrd, Eq, PartialEq, Serialize, Deserialize)]
pub struct Span {
    // Note the ordering of fields for the Ord impl.
    pub file_name: String,
    pub line_start: usize,
    pub column_start: usize,
    pub line_end: usize,
    pub column_end: usize,
}

#[derive(Debug)]
pub struct Def {
    pub kind: raw::DefKind,
    pub span: Span,
    pub name: String,
    pub qualname: String,
    pub parent: Option<u32>,
    pub value: String,
    pub docs: String,
}

impl PerCrateAnalysis {
    pub fn new() -> PerCrateAnalysis {
        PerCrateAnalysis {
            def_id_for_span: HashMap::new(),
            defs: HashMap::new(),
            defs_per_file: HashMap::new(),
            def_names: HashMap::new(),
            ref_spans: HashMap::new(),
            timestamp: None,
        }
    }    
}

impl Analysis {
    pub fn new() -> Analysis {
        Analysis {
            per_crate: HashMap::new(),
            // TODO don't hardcode these
            doc_url_base: "https://doc.rust-lang.org/nightly".to_owned(),
            src_url_base: "https://github.com/rust-lang/rust/blob/master".to_owned(),
        }
    }

    fn timestamps(&self) -> HashMap<PathBuf, Option<SystemTime>> {
        self.per_crate.iter().map(|(s, pc)| (s.clone(), pc.timestamp)).collect()
    }

    fn update(&mut self, per_crate: PerCrateAnalysis, path: PathBuf) {
        self.per_crate.insert(path, per_crate);
    }

    fn has_def(&self, id: u32) -> bool {
        self.for_each_crate(|c| c.defs.get(&id).map(|_| ())).is_some()
    }

    fn for_each_crate<F, T>(&self, f: F) -> Option<T>
        where F: Fn(&PerCrateAnalysis) -> Option<T>
    {
        for (_, ref per_crate) in self.per_crate.iter() {
            if let Some(t) = f(per_crate) {
                return Some(t);
            }
        }

        None
    }

    fn def_id_for_span(&self, span: &Span) -> Option<u32> {
        self.for_each_crate(|c| c.def_id_for_span.get(span).map(|id| *id))
    }

    fn with_defs<F, T>(&self, id: u32, f: F) -> Option<T>
        where F: Fn(&Def) -> T
    {
        self.for_each_crate(|c| c.defs.get(&id).map(&f))
    }

    fn with_defs_and_then<F, T>(&self, id: u32, f: F) -> Option<T>
        where F: Fn(&Def) -> Option<T>
    {
        self.for_each_crate(|c| c.defs.get(&id).and_then(&f))
    }

    fn with_ref_spans<F, T>(&self, id: u32, f: F) -> Option<T>
        where F: Fn(&Vec<Span>) -> T
    {
        self.for_each_crate(|c| c.ref_spans.get(&id).map(&f))
    }

    fn with_defs_per_file<F, T>(&self, file: &str, f: F) -> Option<T>
        where F: Fn(&Vec<u32>) -> T
    {
        self.for_each_crate(|c| c.defs_per_file.get(file).map(&f))
    }

    fn with_def_names<F, T>(&self, name: &str, f: F) -> Option<T>
        where F: Fn(&Vec<u32>) -> T
    {
        self.for_each_crate(|c| c.def_names.get(name).map(&f))
    }

    // TODO resurect these methods
    // pub fn lookup_def_ids(&self, name: &str) -> Option<&Vec<u32>> {
    //     self.def_names.get(name)
    // }

    // fn lookup_def(&self, id: u32) -> &Def {
    //     &self.defs[&id]
    // }

    // pub fn lookup_def_span(&self, id: u32) -> Span {
    //     self.defs[&id].span.clone()
    // }

    // pub fn lookup_refs(&self, id: u32) -> &[Span] {
    //     &self.ref_spans[&id]
    // }

    // pub fn get_spans(&self, id: u32) -> Vec<Span> {
    //     let mut result = self.lookup_refs(id).to_owned();
    //     // TODO what if lookup_def panics
    //     result.push(self.lookup_def(id).span.clone());
    //     result
    // }

    // pub fn get_title(&self, lo: &Loc, hi: &Loc) -> Option<&str> {
    //     let span = Span::from_locs(lo, hi);
    //     self.def_id_for_span.get(&span).and_then(|id| self.defs.get(id).map(|def| &*def.value))
    // }

    // pub fn get_class_id(&self, lo: &Loc, hi: &Loc) -> Option<u32> {
    //     let span = Span::from_locs(lo, hi);
    //     self.def_id_for_span.get(&span).map(|i| *i)
    // }

    // // Basically goto def
    // pub fn get_link(&self, lo: &Loc, hi: &Loc) -> Option<String> {
    //     let span = Span::from_locs(lo, hi);
    //     self.def_id_for_span.get(&span).and_then(|id| self.defs.get(id)).map(|def| {
    //         let s = &def.span;
    //         format!("{}:{}:{}:{}:{}", s.file_name, s.line_start, s.column_start, s.line_end, s.column_end)
    //     })
    // }
}

// impl Span {
//     fn from_locs(lo: &Loc, hi: &Loc) -> Span {
//         Span {
//             file_name: lo.file.name.clone(),
//             line_start: lo.line as usize,
//             column_start: lo.col.0 as usize + 1,
//             line_end: hi.line as usize,
//             column_end: hi.col.0 as usize + 1,
//         }
//     }
// }

// Used to indicate a missing index in the Id.
const NULL: u32 = u32::max_value();
