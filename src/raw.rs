// Copyright 2016 The RLS Project Developers.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use {AnalysisLoader, Blacklist};
use listings::{DirectoryListing, ListingKind};
pub use data::{CratePreludeData, Def, DefKind, Import, Ref, Relation, RelationKind, SigElement,
               Signature, SpanData};
use data::Analysis;


use std::collections::HashMap;
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::time::{Instant, SystemTime};

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

pub fn read_analysis_incremental<L: AnalysisLoader>(
    loader: &L,
    timestamps: HashMap<PathBuf, SystemTime>,
    crate_blacklist: Blacklist,
) -> Vec<Crate> {
    loader.iter_paths(|p| {
        let t = Instant::now();

        let mut result = vec![];

        let listing = match DirectoryListing::from_path(p) {
            Ok(l) => l,
            Err(_) => {
                return result;
            }
        };

        for l in listing.files {
            info!("Considering {:?}", l);
            if let ListingKind::File(ref time) = l.kind {
                if ignore_data(&l.name, crate_blacklist) {
                    continue;
                }

                let path = p.join(&l.name);
                let is_fresh = timestamps.get(&path).map_or(true, |t| time > t);
                if is_fresh {
                    read_crate_data(&path)
                        .map(|a| result.push(Crate::new(a, *time, Some(path))));
                }
            }
        }

        let d = t.elapsed();
        info!(
            "reading {} crates from {} in {}.{:09}s",
            result.len(),
            p.display(),
            d.as_secs(),
            d.subsec_nanos()
        );

        result
    })
}

fn ignore_data(file_name: &str, crate_blacklist: Blacklist) -> bool {
    crate_blacklist.iter()
        .any(|name| file_name.starts_with(&format!("lib{}-", name)))
}

fn read_file_contents(path: &Path) -> Result<String, ::std::io::Error> {
    let mut file = File::open(&path)?;
    let mut buf = String::new();
    file.read_to_string(&mut buf)?;
    Ok(buf)
}

fn read_crate_data(path: &Path) -> Option<Analysis> {
    trace!("read_crate_data {:?}", path);
    let t = Instant::now();

    let buf = read_file_contents(path).or_else(|err| {
        info!("couldn't read file: {}", err);
        Err(err)
    }).ok()?;
    let s = ::rustc_serialize::json::decode(&buf).or_else(|err| {
        info!("deserialisation error: {:?}", err);
        Err(err)
    }).ok()?;

    let d = t.elapsed();
    info!(
        "reading {:?} {}.{:09}s",
        path,
        d.as_secs(),
        d.subsec_nanos()
    );

    s
}

pub fn name_space_for_def_kind(dk: DefKind) -> char {
    match dk {
        DefKind::Enum |
        DefKind::Struct |
        DefKind::Union |
        DefKind::Type |
        DefKind::ExternType |
        DefKind::Trait => 't',
        DefKind::Function |
        DefKind::Method |
        DefKind::Mod |
        DefKind::Local |
        DefKind::Static |
        DefKind::Const |
        DefKind::Tuple |
        DefKind::TupleVariant |
        DefKind::StructVariant |
        DefKind::Field => 'v',
        DefKind::Macro => 'm',
    }
}
