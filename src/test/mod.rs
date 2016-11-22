// Copyright 2016 The RLS Project Developers.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

use {AnalysisHost, AnalysisLoader};

use std::path::{Path, PathBuf};

#[derive(Clone, new)]
struct TestAnalysisLoader {
    path: PathBuf,
}

impl AnalysisLoader for TestAnalysisLoader {
    fn needs_hard_reload(&self, _path_prefix: &Path) -> bool {
        true
    }

    fn fresh_host(&self) -> AnalysisHost<Self> {
        AnalysisHost::new_with_loader(self.clone())
    }

    fn set_path_prefix(&self, _path_prefix: &Path) {
    }

    fn abs_path_prefix(&self) -> Option<PathBuf> {
        panic!();
    }

    fn iter_paths<F, T>(&self, f: F) -> Vec<T>
        where F: Fn(&Path) -> Vec<T>
    {
        let paths = &[&self.path];
        paths.iter().flat_map(|p| f(p).into_iter()).collect()
    }
}


#[test]
fn smoke() {
    // Read in test data and lower it.
    let host = AnalysisHost::new_with_loader(TestAnalysisLoader::new(Path::new("test_data/rls-analysis").to_owned()));
    host.reload(Path::new("test_data/rls-analysis"), false).unwrap();
}
