// Copyright 2016 The RLS Project Developers.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

#[cfg(unix)]
macro_rules! option_try(
    ($e:expr) => (match $e { Some(e) => e, None => return None })
);

#[cfg(unix)]
pub fn get_resident() -> Option<usize> {
    use std::fs::File;
    use std::io::Read;

    let field = 1;
    let mut f = option_try!(File::open("/proc/self/statm").ok());
    let mut contents = String::new();
    option_try!(f.read_to_string(&mut contents).ok());
    let s = option_try!(contents.split_whitespace().nth(field));
    let npages = option_try!(s.parse::<usize>().ok());
    Some(npages * 4096)
}

#[cfg(not(unix))]
pub fn get_resident() -> Option<usize> {
    None
}
