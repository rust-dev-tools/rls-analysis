// Copyright 2016 The RLS Project Developers.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![feature(const_fn)]
#![feature(type_ascription)]
#![feature(proc_macro)]

extern crate rustc_serialize;
extern crate serde;
extern crate serde_json;
#[macro_use]
extern crate serde_derive;

pub mod raw;
mod lowering;
mod listings;

pub use self::raw::Target;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::time::SystemTime;

pub struct AnalysisHost {
    analysis: Mutex<Option<Analysis>>,
    path_prefix: Mutex<Option<PathBuf>>,
    target: Target,
    master_crate_map: Mutex<HashMap<String, u32>>,
}

pub type AResult<T> = Result<T, ()>;

macro_rules! clone_field {
    ($field: ident) => { |x| x.$field.clone() }
}

macro_rules! def_span {
    ($analysis: expr, $id: expr) => {
        $analysis.with_defs_and_then($id, |def| {
            if def.api_crate {
                None
            } else {
                Some(def.span.clone())
            }
        })
    }
}

impl AnalysisHost {
    pub fn new(target: Target) -> AnalysisHost {
        AnalysisHost {
            analysis: Mutex::new(None),
            path_prefix: Mutex::new(None),
            target: target,
            master_crate_map: Mutex::new(HashMap::new()),
        }
    }

    pub fn reload(&self, path_prefix: &Path) -> AResult<()> {
        let mut needs_hard_reload = false;
        match self.path_prefix.lock() {
            Ok(pp) => {
                if pp.is_none() || pp.as_ref().unwrap() != path_prefix {
                    needs_hard_reload = true;
                }
            }
            _ => return Err(()),
        }
        let timestamps = match self.analysis.lock() {
            Ok(a) => {
                match &*a {
                    &Some(ref a) => a.timestamps(),
                    &None => { needs_hard_reload = true; HashMap::new() },
                }
            }
            Err(_) => return Err(()),
        };

        if needs_hard_reload {
            return self.hard_reload(path_prefix);
        }

        let raw_analysis = raw::Analysis::read_incremental(path_prefix, self.target, timestamps);

        lowering::lower(raw_analysis, path_prefix.to_owned(), self, |host, per_crate, path| {
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
    pub fn hard_reload(&self, path_prefix: &Path) -> AResult<()> {
        let raw_analysis = raw::Analysis::read(path_prefix, self.target);

        // We're going to create a dummy AnalysisHost that we will fill with data,
        // then once we're done, we'll swap its data into self.
        let mut new_host = AnalysisHost::new(self.target);
        new_host.analysis = Mutex::new(Some(Analysis::new()));
        lowering::lower(raw_analysis, path_prefix.to_owned(), &mut new_host, |host, per_crate, path| {
            host.analysis.lock().unwrap().as_mut().unwrap().per_crate.insert(path, per_crate);
            Ok(())
        })?;

        match self.path_prefix.lock() {
            Ok(mut pp) => {
                *pp = Some(path_prefix.to_owned());
            }
            Err(_) => return Err(()),
        }
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

    /// Note that self.has_def == true =/> self.goto_def.is_some(), since if the
    /// def is in an api crate, there is no reasonable span to jump to.
    pub fn has_def(&self, id: u32) -> bool {
        match self.analysis.lock() {
            Ok(a) => a.as_ref().unwrap().has_def(id),
            _ => false,
        }
    }

    pub fn goto_def(&self, span: &Span) -> AResult<Span> {
        self.read(|a| {
            a.def_id_for_span(span)
             .and_then(|id| def_span!(a, id))
        })
    }

    pub fn id(&self, span: &Span) -> AResult<u32> {
        self.read(|a| a.def_id_for_span(span))
    }

    pub fn find_all_refs(&self, span: &Span, include_decl: bool) -> AResult<Vec<Span>> {
        if include_decl {
            self.read(|a| {
                a.def_id_for_span(span)
                 .and_then(|id| {
                    a.with_ref_spans(id, |refs| {
                        def_span!(a, id)
                         .into_iter()
                         .chain(refs.iter().cloned())
                         .collect::<Vec<_>>()
                     })
                     .or(def_span!(a, id).map(|s| vec![s]))
                 })
            })
        } else {
            self.read(|a| {
                a.def_id_for_span(span)
                 .map(|id| a.with_ref_spans(id, |refs| refs.clone()).unwrap_or(vec![]))
            })
        }
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

    /// Search for a symbol name, returns a list of spans matching defs and refs
    /// for that name.
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

    // TODO refactor search and find_all_refs to use this
    // Includes all references and the def, the def is always first.
    pub fn find_all_refs_by_id(&self, id: u32) -> AResult<Vec<Span>> {
        self.read(|a| {
            a.with_ref_spans(id, |refs| {
                def_span!(a, id)
                 .into_iter()
                 .chain(refs.iter().cloned())
                 .collect::<Vec<_>>()
             })
             .or(def_span!(a, id).map(|s| vec![s]))
        })
    }

    /// Search for a symbol name, returning a list of def_ids for that name.
    pub fn search_for_id(&self, name: &str) -> AResult<Vec<u32>> {
        self.read(|a| a.with_def_names(name, |defs| defs.clone()))
    }

    pub fn symbols(&self, file_name: &Path) -> AResult<Vec<SymbolResult>> {
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

        // FIXME would be nice not to do this every time.
        let path_prefix = &{
            let p = self.path_prefix.lock().unwrap();
            p.as_ref().map(|ref s| Path::new(s).canonicalize().unwrap().to_owned())
        };

        self.read(|a| {
            a.def_id_for_span(span)
             .and_then(|id| a.with_defs_and_then(id, |def| AnalysisHost::mk_src_url(def, path_prefix.as_ref(), a)))
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
        if !def.api_crate {
            return None;
        }

        if def.parent.is_none() && def.qualname.contains('<') {
            println!("mk_doc_url, bailing, found generic qualname: `{}`", def.qualname);
            return None;
        }

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

    fn mk_src_url(def: &Def, path_prefix: Option<&PathBuf>, analysis: &Analysis) -> Option<String> {
        let path_prefix = match path_prefix {
            Some(pp) => pp,
            None => return None,
        };
        let file_name = &def.span.file_name;
        let file_path = &Path::new(file_name);
        let file_path = match file_path.strip_prefix(&path_prefix) {
            Ok(p) => p,
            Err(_) => return None,
        };

        if def.api_crate {
            Some(format!("{}/{}#L{}-L{}",
                         analysis.src_url_base,
                         file_path.to_str().unwrap(),
                         def.span.line_start + 1,
                         def.span.line_end + 1))
        } else {
            None
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
    defs_per_file: HashMap<PathBuf, Vec<u32>>,
    def_names: HashMap<String, Vec<u32>>,
    ref_spans: HashMap<u32, Vec<Span>>,
    globs: HashMap<Span, Glob>,

    timestamp: Option<SystemTime>,
}

#[derive(Debug, Clone, Hash, Ord, PartialOrd, Eq, PartialEq, Serialize, Deserialize)]
pub struct Span {
    // Note the ordering of fields for the Ord impl.
    pub file_name: PathBuf,
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
    pub api_crate: bool,
    pub parent: Option<u32>,
    pub value: String,
    pub docs: String,
}

#[derive(Debug)]
pub struct Glob {
    pub value: String,
}

impl PerCrateAnalysis {
    pub fn new() -> PerCrateAnalysis {
        PerCrateAnalysis {
            def_id_for_span: HashMap::new(),
            defs: HashMap::new(),
            defs_per_file: HashMap::new(),
            def_names: HashMap::new(),
            ref_spans: HashMap::new(),
            globs: HashMap::new(),
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

    fn with_defs_per_file<F, T>(&self, file: &Path, f: F) -> Option<T>
        where F: Fn(&Vec<u32>) -> T
    {
        self.for_each_crate(|c| c.defs_per_file.get(file).map(&f))
    }

    fn with_def_names<F, T>(&self, name: &str, f: F) -> Option<T>
        where F: Fn(&Vec<u32>) -> T
    {
        self.for_each_crate(|c| c.def_names.get(name).map(&f))
    }
}

// Used to indicate a missing index in the Id.
const NULL: u32 = u32::max_value();
