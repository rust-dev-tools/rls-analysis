// Copyright 2016 The RLS Project Developers.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use AnalysisLoader;
use listings::{DirectoryListing, ListingKind};
pub use data::{Def, DefKind, Ref, CratePreludeData, Signature, SigElement, Import,
               RelationKind, Relation, SpanData};
use data::Analysis;


use std::collections::HashMap;
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, Instant};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Target {
    Release,
    Debug,
}

impl fmt::Display for Target {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Target::Release => write!(f, "release"),
            Target::Debug => write!(f, "debug"),
        }
    }
}

#[derive(Debug, new)]
pub struct Crate {
    pub analysis: Analysis,
    pub timestamp: SystemTime,
    pub path: Option<PathBuf>,
}

pub fn read_analyis_incremental<L: AnalysisLoader>(loader: &L,
                                                   timestamps: HashMap<PathBuf, SystemTime>)
                                                   -> Vec<Crate> {
    loader.iter_paths(|p| {

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

                if loader.ignore_data(&path) {
                    continue;
                }

                match timestamps.get(&path) {
                    Some(t) => {
                        if time > t {
                            read_crate_data(&path).map(|a| result.push(Crate::new(a, *time, Some(path))));
                        }
                    }
                    // A crate we've never seen before.
                    None => {
                        read_crate_data(&path).map(|a| result.push(Crate::new(a, *time, Some(path))));
                    }
                }
            }
        }

        let d = t.elapsed();
        info!("reading {} crates from {} in {}.{:09}s", result.len(), p.display(), d.as_secs(), d.subsec_nanos());

        result
    })
}

fn read_crate_data(path: &Path) -> Option<Analysis> {
    info!("read_crate_data {:?}", path);
    // TODO unwraps
    let t = Instant::now();
    let mut file = File::open(&path).unwrap();
    let mut buf = String::new();
    file.read_to_string(&mut buf).unwrap();
    let s = ::rustc_serialize::json::decode(&buf);

    let d = t.elapsed();
    info!("reading {:?} {}.{:09}s", path, d.as_secs(), d.subsec_nanos());

    if let Err(ref e) = s {
        info!("deserialisation error: {:?}", e);
    }
    s.ok()
}

pub fn name_space_for_def_kind(dk: DefKind) -> char {
    match dk {
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
    }
}
