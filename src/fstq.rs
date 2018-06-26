use fst;

pub fn subsequence<'q>(query: &'q str) -> FstQuery<'q> {
    FstQuery { query, subseq: true }
}

pub fn prefix<'q>(query: &'q str) -> FstQuery<'q> {
    FstQuery { query, subseq: false }
}

#[derive(Clone, Copy)]
pub struct FstQuery<'a> {
    query: &'a str,
    subseq: bool,
}

const NO_MATCH: usize = !0;

impl<'a> fst::Automaton for FstQuery<'a> {
    type State = usize;

    fn start(&self) -> usize {
        0
    }

    fn is_match(&self, &state: &usize) -> bool {
        state == self.query.len()
    }

    fn accept(&self, &state: &usize, byte: u8) -> usize {
        if state == NO_MATCH {
            return state;
        }
        if state == self.query.len() {
            return state;
        }
        if byte == self.query.as_bytes()[state] {
            return state + 1;
        }
        if self.subseq { state } else { NO_MATCH }
    }

    fn can_match(&self, &state: &usize) -> bool {
        state != NO_MATCH
    }

    fn will_always_match(&self, &state: &usize) -> bool {
        state == self.query.len()
    }
}