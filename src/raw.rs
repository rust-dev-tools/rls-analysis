// Copyright 2016 The RLS Project Developers.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use AnalysisLoader;
use listings::{DirectoryListing, ListingKind};

use serde;
use serde::Deserialize;
use serde_json;
use span;

use std::collections::HashMap;
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

#[derive(Deserialize, Debug)]
pub struct Analysis {
    pub kind: Format,
    pub prelude: Option<CratePreludeData>,
    pub imports: Vec<Import>,
    pub defs: Vec<Def>,
    pub refs: Vec<Ref>,
    pub macro_refs: Vec<MacroRef>,
    pub relations: Vec<Relation>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Target {
    Release,
    Debug,
}

#[derive(Deserialize, Copy, Clone, Debug, PartialEq, Eq)]
pub enum Format {
    Csv,
    Json,
    JsonApi,
}

pub struct Crate {
    pub analysis: Analysis,
    pub timestamp: SystemTime,
    pub path: PathBuf,
}

impl Crate {
    fn new(analysis: Analysis, timestamp: SystemTime, path: PathBuf) -> Crate {
        Crate {
            analysis: analysis,
            timestamp: timestamp,
            path: path
        }
    }
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Target::Release => write!(f, "release"),
            Target::Debug => write!(f, "debug"),
        }
    }
}

impl Analysis {
    pub fn read_incremental<L: AnalysisLoader>(loader: &L,
                                               timestamps: HashMap<PathBuf, Option<SystemTime>>)
                                               -> Vec<Crate> {
        loader.iter_paths(|p| {
            use std::time::*;

            let t = Instant::now();

            let mut result = vec![];

            let listing = match DirectoryListing::from_path(p) {
                Ok(l) => l,
                Err(_) => { return result; },
            };

            for l in listing.files {
                info!{"Considering {:?}", l}
                if let ListingKind::File(ref time) = l.kind {
                    let mut path = p.to_path_buf();
                    path.push(&l.name);

                    match timestamps.get(&path) {
                        Some(&Some(ref t)) => {
                            if time > t {
                                Self::read_crate_data(&path).map(|a| result.push(Crate::new(a, *time, path)));
                            }
                        }
                        // A crate we should never need to refresh.
                        Some(&None) => {}
                        // A crate we've never seen before.
                        None => {
                            Self::read_crate_data(&path).map(|a| result.push(Crate::new(a, *time, path)));
                        }
                    }
                }
            }

            let _d = t.elapsed();
            // println!("reading {} crates from {} in {}.{:09}s", result.len(), p.display(), _d.as_secs(), _d.subsec_nanos());

            result
        })
    }

    pub fn read<L: AnalysisLoader>(loader: &L) -> Vec<Crate> {
        Self::read_incremental(loader, HashMap::new())
    }

    fn read_crate_data(path: &Path) -> Option<Analysis> {
        info!("read_crate_data {:?}", path);
        // TODO unwraps
        let mut file = File::open(&path).unwrap();
        let mut buf = String::new();
        file.read_to_string(&mut buf).unwrap();
        serde_json::from_str(&buf).ok()
    }
}

#[derive(Deserialize, Debug)]
pub struct CompilerId {
    pub krate: u32,
    pub index: u32,
}

#[derive(Deserialize, Debug)]
pub struct CratePreludeData {
    pub crate_name: String,
    pub crate_root: String,
    pub external_crates: Vec<ExternalCrateData>,
    pub span: SpanData,
}

#[derive(Deserialize, Debug)]
pub struct ExternalCrateData {
    pub name: String,
    pub num: u32,
    pub file_name: String,
}

#[derive(Deserialize, Debug)]
pub struct Def {
    pub kind: DefKind,
    pub id: CompilerId,
    pub span: SpanData,
    pub name: String,
    pub qualname: String,
    pub parent: Option<CompilerId>,
    pub children: Option<Vec<CompilerId>>,
    pub value: String,
    pub docs: String,
    pub sig: Option<Signature>,
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Serialize)]
pub enum DefKind {
    Enum,
    Tuple,
    Struct,
    Union,
    Trait,
    Function,
    Method,
    Macro,
    Mod,
    Type,
    Local,
    Static,
    Const,
    Field,
    Import,
}

impl DefKind {
    pub fn name_space(&self) -> char {
        match *self {
            DefKind::Enum |
            DefKind::Tuple |
            DefKind::Struct |
            DefKind::Union |
            DefKind::Type |
            DefKind::Trait => 't',
            DefKind::Function |
            DefKind::Method |
            DefKind::Mod |
            DefKind::Local |
            DefKind::Static |
            DefKind::Const |
            DefKind::Field => 'v',
            DefKind::Macro => 'm',
            DefKind::Import => { panic!("No namespace for imports"); }
        }
    }
}

// Custom impl to read rustc_serialize's format.
impl Deserialize for DefKind {
    fn deserialize<D>(deserializer: D) -> Result<DefKind, D::Error>
        where D: serde::Deserializer,
    {
        let s = String::deserialize(deserializer)?;
        match &*s {
            "Enum" => Ok(DefKind::Enum),
            "Tuple" => Ok(DefKind::Tuple),
            "Struct" => Ok(DefKind::Struct),
            "Union" => Ok(DefKind::Union),
            "Trait" => Ok(DefKind::Trait),
            "Function" => Ok(DefKind::Function),
            "Method" => Ok(DefKind::Method),
            "Macro" => Ok(DefKind::Macro),
            "Mod" => Ok(DefKind::Mod),
            "Type" => Ok(DefKind::Type),
            "Local" => Ok(DefKind::Local),
            "Static" => Ok(DefKind::Static),
            "Const" => Ok(DefKind::Const),
            "Field" => Ok(DefKind::Field),
            _ => {
                Err(serde::de::Error::custom("unexpected def kind"))
            }
        }
    }
}

#[derive(Debug, Deserialize)]
pub struct Signature {
    pub span: SpanData,
    pub text: String,
    pub ident_start: usize,
    pub ident_end: usize,
    pub defs: Vec<SigElement>,
    pub refs: Vec<SigElement>,
}

#[derive(Debug, Deserialize)]
pub struct SigElement {
    pub id: CompilerId,
    pub start: usize,
    pub end: usize,
}

#[derive(Deserialize, Debug)]
pub struct Ref {
    pub kind: RefKind,
    pub span: SpanData,
    pub ref_id: CompilerId,
}

#[derive(Debug)]
pub enum RefKind {
    Function,
    Mod,
    Type,
    Variable,
}

// Custom impl to read rustc_serialize's format.
impl Deserialize for RefKind {
    fn deserialize<D>(deserializer: D) -> Result<RefKind, D::Error>
        where D: serde::Deserializer,
    {
        let s = String::deserialize(deserializer)?;
        match &*s {
            "Function" => Ok(RefKind::Function),
            "Mod" => Ok(RefKind::Mod),
            "Type" => Ok(RefKind::Type),
            "Variable" => Ok(RefKind::Variable),
            _ => Err(serde::de::Error::custom("unexpected ref kind")),
        }
    }
}

#[derive(Deserialize, Debug)]
pub struct MacroRef {
    pub span: SpanData,
    pub qualname: String,
    pub callee_span: SpanData,
}

#[derive(Deserialize, Debug)]
pub struct Import {
    pub kind: ImportKind,
    pub ref_id: Option<CompilerId>,
    pub span: SpanData,
    pub name: String,
    pub value: String,
}

#[derive(Debug)]
pub enum ImportKind {
    ExternCrate,
    Use,
    GlobUse,
}

// Custom impl to read rustc_serialize's format.
impl Deserialize for ImportKind {
    fn deserialize<D>(deserializer: D) -> Result<ImportKind, D::Error>
        where D: serde::Deserializer,
    {
        let s = String::deserialize(deserializer)?;
        match &*s {
            "ExternCrate" => Ok(ImportKind::ExternCrate),
            "Use" => Ok(ImportKind::Use),
            "GlobUse" => Ok(ImportKind::GlobUse),
            _ => Err(serde::de::Error::custom("unexpected import kind")),
        }
    }
}

#[derive(Deserialize, Debug)]
pub struct Relation {
    pub span: SpanData,
    pub kind: RelationKind,
    pub from: CompilerId,
    pub to: CompilerId,
}

#[derive(Debug, Eq, PartialEq)]
pub enum RelationKind {
    Impl,
    SuperTrait,
}

// Custom impl to read rustc_serialize's format.
impl Deserialize for RelationKind {
    fn deserialize<D>(deserializer: D) -> Result<RelationKind, D::Error>
        where D: serde::Deserializer,
    {
        let s = String::deserialize(deserializer)?;
        match &*s {
            "Impl" => Ok(RelationKind::Impl),
            "SuperTrait" => Ok(RelationKind::SuperTrait),
            _ => Err(serde::de::Error::custom("unexpected relation kind")),
        }
    }
}

#[derive(Deserialize, Debug, Clone)]
pub struct SpanData {
    pub file_name: PathBuf,
    pub byte_start: u32,
    pub byte_end: u32,
    pub line_start: span::Row<span::OneIndexed>,
    pub line_end: span::Row<span::OneIndexed>,
    // Character offset.
    pub column_start: span::Column<span::OneIndexed>,
    pub column_end: span::Column<span::OneIndexed>,
}
