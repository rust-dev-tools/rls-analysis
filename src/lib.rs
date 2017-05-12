// Copyright 2016 The RLS Project Developers.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#![feature(const_fn)]
#![feature(type_ascription)]

#[macro_use]
extern crate derive_new;
#[macro_use]
extern crate log;
extern crate rls_data as data;
extern crate rls_span as span;
extern crate rustc_serialize;

pub mod raw;
mod lowering;
mod listings;
mod util;
#[cfg(test)]
mod test;

pub use self::raw::{Target, name_space_for_def_kind, read_analyis_incremental};

use std::collections::HashMap;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Mutex;
use std::time::{Instant, SystemTime};

pub struct AnalysisHost<L: AnalysisLoader = CargoAnalysisLoader> {
    analysis: Mutex<Option<Analysis>>,
    master_crate_map: Mutex<HashMap<String, u32>>,
    loader: L,
}

pub struct CargoAnalysisLoader {
    path_prefix: Mutex<Option<PathBuf>>,
    target: Target,
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

pub trait AnalysisLoader: Sized {
    fn needs_hard_reload(&self, path_prefix: &Path) -> bool;
    fn fresh_host(&self) -> AnalysisHost<Self>;
    fn set_path_prefix(&self, path_prefix: &Path);
    fn abs_path_prefix(&self) -> Option<PathBuf>;
    fn iter_paths<F, T>(&self, f: F) -> Vec<T>
        where F: Fn(&Path) -> Vec<T>;
}

impl AnalysisLoader for CargoAnalysisLoader {
    fn needs_hard_reload(&self, path_prefix: &Path) -> bool {
        let pp = self.path_prefix.lock().unwrap();
        pp.is_none() || pp.as_ref().unwrap() != path_prefix
    }

    fn fresh_host(&self) -> AnalysisHost<Self> {
        let pp = self.path_prefix.lock().unwrap();
        AnalysisHost::new_with_loader(CargoAnalysisLoader {
            path_prefix: Mutex::new(pp.clone()),
            target: self.target,
        })
    }

    fn set_path_prefix(&self, path_prefix: &Path) {
        let mut pp = self.path_prefix.lock().unwrap();
        *pp = Some(path_prefix.to_owned())
    }

    fn abs_path_prefix(&self) -> Option<PathBuf> {
        let p = self.path_prefix.lock().unwrap();
        p.as_ref().map(|s| Path::new(s).canonicalize().unwrap().to_owned())
    }

    fn iter_paths<F, T>(&self, f: F) -> Vec<T>
        where F: Fn(&Path) -> Vec<T>
    {
        let path_prefix = self.path_prefix.lock().unwrap();
        let path_prefix = path_prefix.as_ref().unwrap();
        let target = self.target.to_string();

        // TODO deps path allows to break out of 'sandbox' - is that Ok?
        let principle_path = path_prefix.join("target").join("rls").join(&target).join("save-analysis");
        let deps_path = path_prefix.join("target").join("rls").join(&target).join("deps").join("save-analysis");
        let sys_root_path = sys_root_path();
        let target_triple = extract_target_triple(sys_root_path.as_path());
        let libs_path = sys_root_path
            .join("lib")
            .join("rustlib")
            .join(&target_triple)
            .join("analysis");
        let paths = &[&libs_path,
                      &deps_path,
                      &principle_path];

        paths.iter().flat_map(|p| f(p).into_iter()).collect()
    }
}

fn extract_target_triple(sys_root_path: &Path) -> String {
    // Extracts nightly-x86_64-pc-windows-msvc from $HOME/.rustup/toolchains/nightly-x86_64-pc-windows-msvc
    let toolchain = sys_root_path.iter()
                                 .last()
                                 .and_then(OsStr::to_str)
                                 .expect("extracting toolchain failed");
    // Extracts x86_64-pc-windows-msvc from nightly-x86_64-pc-windows-pc
    let triple = toolchain.splitn(2, "-")
                          .last()
                          .map(String::from)
                          .expect("extracting triple failed");
    triple
}

fn sys_root_path() -> PathBuf {
    option_env!("SYSROOT")
        .map(PathBuf::from)
        .or_else(|| {
            Command::new("rustc")
            .arg("--print")
            .arg("sysroot")
            .output()
            .ok()
            .and_then(|out| String::from_utf8(out.stdout).ok())
            .map(|s| PathBuf::from(s.trim()))
        })
        .expect("need to specify SYSROOT env var, \
                 or rustc must be in PATH")
}

impl AnalysisHost<CargoAnalysisLoader> {
    pub fn new(target: Target) -> AnalysisHost {
        AnalysisHost {
            analysis: Mutex::new(None),
            master_crate_map: Mutex::new(HashMap::new()),
            loader: CargoAnalysisLoader {
                path_prefix: Mutex::new(None),
                target: target,
            }
        }
    }
}

impl<L: AnalysisLoader> AnalysisHost<L> {
    pub fn new_with_loader(l: L) -> AnalysisHost<L> {
        AnalysisHost {
            analysis: Mutex::new(None),
            master_crate_map: Mutex::new(HashMap::new()),
            loader: l,
        }
    }

    /// Reloads given data passed in `analysis`. This will first check and read
    /// on-disk data (just like `reload`). It then imports the data we're
    /// passing in directly.
    pub fn reload_from_analysis(&self,
                                analysis: data::Analysis,
                                path_prefix: &Path,
                                base_dir: &Path,
                                full_docs: bool)
                                -> AResult<()> {
        self.reload(path_prefix, base_dir, full_docs)?;

        lowering::lower(vec![raw::Crate::new(analysis, SystemTime::now(), None)],
                        base_dir,
                        full_docs,
                        self,
                        |host, per_crate, path| {
            let mut a = host.analysis.lock().map_err(|_| ())?;
            a.as_mut().unwrap().update(per_crate, path);
            Ok(())
        })
    }

    pub fn reload(&self, path_prefix: &Path, base_dir: &Path, full_docs: bool) -> AResult<()> {
        info!("reload {:?} {:?}", path_prefix, base_dir);
        let empty = {
            let a = self.analysis.lock().map_err(|_| ())?;
            a.is_none()
        };
        if empty || self.loader.needs_hard_reload(path_prefix) {
            return self.hard_reload(path_prefix, base_dir, full_docs);
        }

        let timestamps = {
            let a = self.analysis.lock().map_err(|_| ())?;
            a.as_ref().unwrap().timestamps()
        };

        let raw_analysis = read_analyis_incremental(&self.loader, timestamps);

        lowering::lower(raw_analysis, base_dir, full_docs, self, |host, per_crate, path| {
            let mut a = host.analysis.lock().map_err(|_| ())?;
            a.as_mut().unwrap().update(per_crate, path);
            Ok(())
        })
    }

    // Reloads the entire project's analysis data.
    pub fn hard_reload(&self, path_prefix: &Path, base_dir: &Path, full_docs: bool) -> AResult<()> {
        self.loader.set_path_prefix(path_prefix);
        let raw_analysis = read_analyis_incremental(&self.loader, HashMap::new());

        // We're going to create a dummy AnalysisHost that we will fill with data,
        // then once we're done, we'll swap its data into self.
        let mut fresh_host = self.loader.fresh_host();
        fresh_host.analysis = Mutex::new(Some(Analysis::new()));
        let lowering_result = lowering::lower(raw_analysis, base_dir, full_docs, &fresh_host, |host, per_crate, path| {
            host.analysis.lock().unwrap().as_mut().unwrap().per_crate.insert(path, per_crate);
            Ok(())
        });

        if let Err(s) = lowering_result {
            let mut a = self.analysis.lock().map_err(|_| ())?;
            *a = None;
            return Err(s);
        }

        {
            let mut mcm = self.master_crate_map.lock().map_err(|_| ())?;
            *mcm = fresh_host.master_crate_map.into_inner().unwrap();
        }

        let mut a = self.analysis.lock().map_err(|_| ())?;
        *a = Some(fresh_host.analysis.into_inner().unwrap().unwrap());
        Ok(())
    }

    /// Note that self.has_def == true =/> self.goto_def.is_some(), since if the
    /// def is in an api crate, there is no reasonable span to jump to.
    pub fn has_def(&self, id: u32) -> bool {
        match self.analysis.lock() {
            Ok(a) => a.as_ref().unwrap().has_def(id),
            _ => false,
        }
    }

    pub fn get_def(&self, id: u32) -> AResult<Def> {
        self.with_analysis(|a| a.with_defs(id, |def| def.clone()))
    }

    pub fn goto_def(&self, span: &Span) -> AResult<Span> {
        self.with_analysis(|a| {
            a.def_id_for_span(span)
             .and_then(|id| def_span!(a, id))
        })
    }

    pub fn for_each_child_def<F, T>(&self, id: u32, f: F) -> AResult<Vec<T>>
        where F: Fn(u32, &Def) -> T
    {
        self.with_analysis(|a| a.for_each_child(id, f))
    }

    pub fn def_parents(&self, id: u32) -> AResult<Vec<(u32, String)>> {
        self.with_analysis(|a| {
            let mut result = vec![];
            let mut next = id;
            loop {
                match a.with_defs_and_then(next, |def| def.parent.and_then(|p| {
                    a.with_defs(p, |def| (p, def.name.clone()))
                })) {
                    Some((id, name)) => {
                        result.insert(0, (id, name));
                        next = id;
                    }
                    None => {
                        return Some(result);
                    }
                }
            }
        })
    }

    /// Returns the name of each crate in the program and the id of the root
    /// module of that crate.
    pub fn def_roots(&self) -> AResult<Vec<(u32, String)>> {
        self.with_analysis(|a| {
            Some(a.for_all_crates(|c| c.root_id.map(|id| {
                vec![(id, c.name.clone())]
            })))
        })
    }

    pub fn id(&self, span: &Span) -> AResult<u32> {
        self.with_analysis(|a| a.def_id_for_span(span))
    }

    pub fn find_all_refs(&self, span: &Span, include_decl: bool) -> AResult<Vec<Span>> {
        let t_start = Instant::now();
        let result = if include_decl {
            self.with_analysis(|a| {
                a.def_id_for_span(span)
                 .and_then(|id| {
                    a.with_ref_spans(id, |refs| {
                        def_span!(a, id)
                         .into_iter()
                         .chain(refs.iter().cloned())
                         .collect::<Vec<_>>()
                     })
                     .or_else(|| def_span!(a, id).map(|s| vec![s]))
                 })
            })
        } else {
            self.with_analysis(|a| {
                a.def_id_for_span(span)
                 .map(|id| {
                    a.with_ref_spans(id, |refs| refs.clone())
                     .unwrap_or_else(Vec::new)
                 })
            })
        };

        let time = t_start.elapsed();
        info!("find_all_refs: {}s", time.as_secs() as f64 + time.subsec_nanos() as f64 / 1_000_000_000.0);
        result
    }

    pub fn show_type(&self, span: &Span) -> AResult<String> {
        self.with_analysis(|a| {
            a.def_id_for_span(span)
             .and_then(|id| a.with_defs(id, clone_field!(value)))
             .or_else(|| a.with_globs(span, clone_field!(value)))
        })
    }

    pub fn docs(&self, span: &Span) -> AResult<String> {
        self.with_analysis(|a| {
            a.def_id_for_span(span)
             .and_then(|id| a.with_defs(id, clone_field!(docs)))
         })
    }

    /// Search for a symbol name, returns a list of spans matching defs and refs
    /// for that name.
    pub fn search(&self, name: &str) -> AResult<Vec<Span>> {
        let t_start = Instant::now();
        let result = self.with_analysis(|a| {
            Some(a.with_def_names(name, |defs| {
                info!("defs: {:?}", defs);
                defs.into_iter()
                    .flat_map(|id| {
                        a.with_ref_spans(*id, |refs|
                            {
                            def_span!(a, *id)
                             .into_iter()
                             .chain(refs.iter().cloned())
                             .collect::<Vec<_>>()})
                         .or_else(|| def_span!(a, *id).map(|s| vec![s]))
                         .unwrap_or_else(Vec::new)
                         .into_iter()
                     })
                     .collect(): Vec<Span>
             }))
        });

        let time = t_start.elapsed();
        info!("search: {}s", time.as_secs() as f64 + time.subsec_nanos() as f64 / 1_000_000_000.0);
        result
    }

    // TODO refactor search and find_all_refs to use this
    // Includes all references and the def, the def is always first.
    pub fn find_all_refs_by_id(&self, id: u32) -> AResult<Vec<Span>> {
        let t_start = Instant::now();
        let result = self.with_analysis(|a| {
            a.with_ref_spans(id, |refs| {
                def_span!(a, id)
                 .into_iter()
                 .chain(refs.iter().cloned())
                 .collect::<Vec<_>>()
             })
             .or_else(|| def_span!(a, id).map(|s| vec![s]))
        });

        let time = t_start.elapsed();
        info!("find_all_refs_by_id: {}s", time.as_secs() as f64 + time.subsec_nanos() as f64 / 1_000_000_000.0);
        result
    }

    pub fn find_impls(&self, id: u32) -> AResult<Vec<Span>> {
        self.with_analysis(|a| Some(a.for_all_crates(|c| c.impls.get(&id).map(|v| v.clone()))))
    }

    /// Search for a symbol name, returning a list of def_ids for that name.
    pub fn search_for_id(&self, name: &str) -> AResult<Vec<u32>> {
        self.with_analysis(|a| Some(a.with_def_names(name, |defs| defs.clone())))
    }

    pub fn symbols(&self, file_name: &Path) -> AResult<Vec<SymbolResult>> {
        self.with_analysis(|a| {
            a.with_defs_per_file(file_name, |ids| {
                ids.iter()
                   .map(|id| a.with_defs(*id, |def| SymbolResult::new(*id, def)).unwrap())
                   .collect()
            })
        })
    }

    pub fn doc_url(&self, span: &Span) -> AResult<String> {
        // e.g., https://doc.rust-lang.org/nightly/std/string/String.t.html
        self.with_analysis(|a| {
            a.def_id_for_span(span)
             .and_then(|id| a.with_defs_and_then(id, |def| AnalysisHost::<L>::mk_doc_url(def, a)))
        })
    }

    pub fn src_url(&self, span: &Span) -> AResult<String> {
        // e.g., https://github.com/rust-lang/rust/blob/master/src/libcollections/string.rs#L261-L263

        // FIXME would be nice not to do this every time.
        let path_prefix = &self.loader.abs_path_prefix();

        self.with_analysis(|a| {
            a.def_id_for_span(span)
             .and_then(|id| a.with_defs_and_then(id, |def| AnalysisHost::<L>::mk_src_url(def, path_prefix.as_ref(), a)))
        })
    }

    fn with_analysis<F, T>(&self, f: F) -> AResult<T>
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
            _ => Err(()),
        }
    }

    fn mk_doc_url(def: &Def, analysis: &Analysis) -> Option<String> {
        if !def.api_crate {
            return None;
        }

        if def.parent.is_none() && def.qualname.contains('<') {
            debug!("mk_doc_url, bailing, found generic qualname: `{}`", def.qualname);
            return None;
        }

        match def.parent {
            Some(p) => {
                analysis.with_defs(p, |parent| {
                    let parent_qualpath = parent.qualname.replace("::", "/");
                    let ns = name_space_for_def_kind(def.kind);
                    format!("{}/{}.t.html#{}.{}", analysis.doc_url_base, parent_qualpath, def.name, ns)
                })
            }
            None => {
                let qualpath = def.qualname.replace("::", "/");
                let ns = name_space_for_def_kind(def.kind);
                Some(format!("{}/{}.{}.html", analysis.doc_url_base, qualpath, ns))
            }
        }
    }

    fn mk_src_url(def: &Def, path_prefix: Option<&PathBuf>, analysis: &Analysis) -> Option<String> {
        let path_prefix = match path_prefix {
            Some(pp) => pp,
            None => return None,
        };
        let file_path = &def.span.file;
        let file_path = match file_path.strip_prefix(&path_prefix) {
            Ok(p) => p,
            Err(_) => return None,
        };

        if def.api_crate {
            Some(format!("{}/{}#L{}-L{}",
                         analysis.src_url_base,
                         file_path.to_str().unwrap(),
                         def.span.range.row_start.one_indexed().0,
                         def.span.range.row_end.one_indexed().0))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
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
            kind: def.kind,
        }
    }
}

type Span = span::Span<span::ZeroIndexed>;

#[derive(Debug)]
pub struct Analysis {
    // The primary crate will have its data passed directly, not via a file, so
    // there is no path for it. Because of this key into the hashmap, this means
    // we can only pass the data for one crate directly.
    per_crate: HashMap<Option<PathBuf>, PerCrateAnalysis>,

    pub doc_url_base: String,
    pub src_url_base: String,
}

#[derive(Debug)]
pub struct PerCrateAnalysis {
    // Map span to id of def (either because it is the span of the def, or of the def for the ref).
    def_id_for_span: HashMap<Span, u32>,
    defs: HashMap<u32, Def>,
    defs_per_file: HashMap<PathBuf, Vec<u32>>,
    children: HashMap<u32, Vec<u32>>,
    def_names: HashMap<String, Vec<u32>>,
    ref_spans: HashMap<u32, Vec<Span>>,
    globs: HashMap<Span, Glob>,
    impls: HashMap<u32, Vec<Span>>,

    name: String,
    root_id: Option<u32>,
    timestamp: Option<SystemTime>,
}

#[derive(Debug, Clone)]
pub struct Def {
    pub kind: raw::DefKind,
    pub span: Span,
    pub name: String,
    pub qualname: String,
    pub api_crate: bool,
    pub parent: Option<u32>,
    pub value: String,
    pub docs: String,
    pub sig: Option<Signature>,
}

#[derive(Debug, Clone)]
pub struct Signature {
    pub span: Span,
    pub text: String,
    pub ident_start: u32,
    pub ident_end: u32,
    pub defs: Vec<SigElement>,
    pub refs: Vec<SigElement>,
}

#[derive(Debug, Clone)]
pub struct SigElement {
    pub id: u32,
    pub start: usize,
    pub end: usize,
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
            children: HashMap::new(),
            def_names: HashMap::new(),
            ref_spans: HashMap::new(),
            globs: HashMap::new(),
            impls: HashMap::new(),
            name: String::new(),
            root_id: None,
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
        self.per_crate.iter().filter_map(|(s, pc)| s.as_ref().map(|s| (s.clone(), pc.timestamp))).collect()
    }

    fn update(&mut self, per_crate: PerCrateAnalysis, path: Option<PathBuf>) {
        self.per_crate.insert(path, per_crate);
    }

    fn has_def(&self, id: u32) -> bool {
        self.for_each_crate(|c| c.defs.get(&id).map(|_| ())).is_some()
    }

    fn for_each_crate<F, T>(&self, f: F) -> Option<T>
        where F: Fn(&PerCrateAnalysis) -> Option<T>
    {
        for per_crate in self.per_crate.values() {
            if let Some(t) = f(per_crate) {
                return Some(t);
            }
        }

        None
    }

    fn for_all_crates<F, T>(&self, f: F) -> Vec<T>
        where F: Fn(&PerCrateAnalysis) -> Option<Vec<T>>
    {
        let mut result = vec![];
        for per_crate in self.per_crate.values() {
            if let Some(this_crate) = f(per_crate) {
                result.extend(this_crate);
            }
        }

        result
    }

    fn def_id_for_span(&self, span: &Span) -> Option<u32> {
        self.for_each_crate(|c| c.def_id_for_span.get(span).cloned())
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

    fn with_globs<F, T>(&self, span: &Span, f: F) -> Option<T>
        where F: Fn(&Glob) -> T
    {
        self.for_each_crate(|c| c.globs.get(span).map(&f))
    }

    fn for_each_child<F, T>(&self, id: u32, f: F) -> Option<Vec<T>>
        where F: Fn(u32, &Def) -> T
    {
        for per_crate in self.per_crate.values() {
            if let Some(children) = per_crate.children.get(&id) {
                return Some(children.iter().map(|id| f(*id, &per_crate.defs[id])).collect());
            }
        }

        Some(vec![])
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

    fn with_def_names<F, T>(&self, name: &str, f: F) -> Vec<T>
        where F: Fn(&Vec<u32>) -> Vec<T>
    {
        self.for_all_crates(|c| c.def_names.get(name).map(&f))
    }
}

// Used to indicate a missing index in the Id.
const NULL: u32 = u32::max_value();

#[cfg(test)]
mod tests {
    mod extract_target_triple {
        use std::path::Path;

        #[test]
        fn windows_path() {
            let path = Path::new(r#"C:\Users\user\.rustup\toolchains\nightly-x86_64-pc-windows-msvc"#);
            assert_eq!(::extract_target_triple(path), String::from("x86_64-pc-windows-msvc"));
        }

        #[test]
        fn unix_path() {
            let path = Path::new("/home/user/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu");
            assert_eq!(::extract_target_triple(path), String::from("x86_64-unknown-linux-gnu"));
        }
    }
}
