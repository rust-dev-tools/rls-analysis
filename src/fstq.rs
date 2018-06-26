use fst;

pub fn subsequence<'q>(query: &'q str) -> impl fst::Automaton + 'q {
    FstSubSeq { query }
}

pub fn prefix<'q>(query: &'q str) -> impl fst::Automaton + 'q {
    FstPrefix { query }
}


#[derive(Clone, Copy)]
struct FstSubSeq<'a> { query: &'a str }

impl<'a> fst::Automaton for FstSubSeq<'a> {
    type State = usize;

    fn start(&self) -> usize {
        0
    }

    fn is_match(&self, &state: &usize) -> bool {
        state == self.query.len()
    }

    fn accept(&self, &state: &usize, byte: u8) -> usize {
        if state >= self.query.len() {
            return state;
        }
        if byte == self.query.as_bytes()[state] {
            return state + 1;
        }
        return state;
    }

    fn can_match(&self, _: &usize) -> bool {
        true
    }

    fn will_always_match(&self, &state: &usize) -> bool {
        state == self.query.len()
    }
}


#[derive(Clone, Copy)]
struct FstPrefix<'a> { query: &'a str }

const NO_MATCH: usize = !0;

impl<'a> fst::Automaton for FstPrefix<'a> {
    type State = usize;

    fn start(&self) -> usize {
        0
    }

    fn is_match(&self, &state: &usize) -> bool {
        state == self.query.len()
    }

    fn accept(&self, &state: &usize, byte: u8) -> usize {
        if state == NO_MATCH {
            return NO_MATCH;
        }
        if state == self.query.len() {
            return state;
        }
        if byte == self.query.as_bytes()[state] {
            return state + 1;
        }
        return NO_MATCH;
    }

    fn can_match(&self, &state: &usize) -> bool {
        state != NO_MATCH
    }

    fn will_always_match(&self, &state: &usize) -> bool {
        state == self.query.len()
    }
}