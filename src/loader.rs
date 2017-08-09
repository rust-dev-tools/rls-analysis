// Copyright 2017 The RLS Project Developers.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Mutex;

use raw::Target;
use AnalysisHost;

#[derive(Debug)]
pub struct CargoAnalysisLoader {
    pub path_prefix: Mutex<Option<PathBuf>>,
    pub target: Target,
}

pub trait AnalysisLoader: Sized {
    fn needs_hard_reload(&self, path_prefix: &Path) -> bool;
    fn fresh_host(&self) -> AnalysisHost<Self>;
    fn set_path_prefix(&self, path_prefix: &Path);
    fn abs_path_prefix(&self) -> Option<PathBuf>;
    fn iter_paths<F, T>(&self, f: F) -> Vec<T>
    where
        F: Fn(&Path) -> Vec<T>;
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
        p.as_ref()
            .map(|s| Path::new(s).canonicalize().unwrap().to_owned())
    }

    fn iter_paths<F, T>(&self, f: F) -> Vec<T>
    where
        F: Fn(&Path) -> Vec<T>,
    {
        let path_prefix = self.path_prefix.lock().unwrap();
        let path_prefix = path_prefix.as_ref().unwrap();
        let target = self.target.to_string();

        // TODO sys_root_path allows to break out of 'sandbox' - is that Ok?
        let principle_path = path_prefix
            .join("target")
            .join("rls")
            .join(&target)
            .join("save-analysis");
        let deps_path = path_prefix
            .join("target")
            .join("rls")
            .join(&target)
            .join("deps")
            .join("save-analysis");
        let sys_root_path = sys_root_path();
        let target_triple = extract_target_triple(sys_root_path.as_path());
        let libs_path = sys_root_path
            .join("lib")
            .join("rustlib")
            .join(&target_triple)
            .join("analysis");
        let paths = &[&libs_path, &deps_path, &principle_path];

        paths.iter().flat_map(|p| f(p).into_iter()).collect()
    }
}

fn extract_target_triple(sys_root_path: &Path) -> String {
    // Extracts nightly-x86_64-pc-windows-msvc from
    // $HOME/.rustup/toolchains/nightly-x86_64-pc-windows-msvc
    let toolchain = sys_root_path
        .iter()
        .last()
        .and_then(OsStr::to_str)
        .expect("extracting toolchain failed");
    // Extracts x86_64-pc-windows-msvc from nightly-x86_64-pc-windows-pc
    let triple = toolchain
        .splitn(2, "-")
        .last()
        .map(String::from)
        .expect("extracting triple failed");
    triple
}

fn sys_root_path() -> PathBuf {
    option_env!("SYSROOT")
        .map(PathBuf::from)
        .or_else(|| {
            option_env!("RUSTC").and_then(|rustc| {
                Command::new(rustc)
                    .arg("--print")
                    .arg("sysroot")
                    .output()
                    .ok()
                    .and_then(|out| String::from_utf8(out.stdout).ok())
                    .map(|s| PathBuf::from(s.trim()))
            })
        })
        .or_else(|| {
            Command::new("rustc")
                .arg("--print")
                .arg("sysroot")
                .output()
                .ok()
                .and_then(|out| String::from_utf8(out.stdout).ok())
                .map(|s| PathBuf::from(s.trim()))
        })
        .expect(
            "need to specify SYSROOT or RUSTC env vars, \
             or rustc must be in PATH",
        )
}

#[cfg(test)]
mod tests {
    use std::path::Path;
    use super::*;

    #[test]
    fn windows_path() {
        let path = Path::new(
            r#"C:\Users\user\.rustup\toolchains\nightly-x86_64-pc-windows-msvc"#,
        );
        assert_eq!(
            extract_target_triple(path),
            String::from("x86_64-pc-windows-msvc")
        );
    }

    #[test]
    fn unix_path() {
        let path = Path::new(
            "/home/user/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu",
        );
        assert_eq!(
            extract_target_triple(path),
            String::from("x86_64-unknown-linux-gnu")
        );
    }
}
