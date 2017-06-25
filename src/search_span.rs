use std::borrow::Borrow;
use std::cmp::Ordering;
use std::ops::Range;
use super::Span;

///! These types are used to assist in finding a range of items in a BTreeMap when
///! the keys to the BTreeMap are spans. You are supposed to use the type InTreeSpan
///! as the key for the BTreeMap, and then use the struct created from SearchSpan::range
///! to call the BTreeMap's range method. Here's a graphical explanation of how this
///! works:
///!     |-----|     <- span used for SearchSpan::range
///!                    The spans below are all keys in the BTreeMap
///!                    In Result? Reason
///! |-------------| <- Yes        (wraps the SearchSpan) \ 
///!    |-------|    <- Yes        (wraps the SearchSpan) - up to you which results to use
///!   |----|        <- No         (only wraps the start of the SearchSpan)
///!       |-----|   <- No         (only wraps the end of the SearchSpan)
///! |-|             <- No         (doesn't wrap)
///!             |-| <- No         (doesn't wrap)

#[derive(Debug)]
enum SpanKind {
    InTree, // Span keys tracked inside the BTreeMap
    Start,  // Start range to search for
    End     // End range to search for
}

#[derive(Debug)]
pub struct InTreeSpan(SearchSpan);

impl InTreeSpan {
    pub fn new(span: &Span) -> Self {
        InTreeSpan(SearchSpan(span.clone(), SpanKind::InTree))
    }

    pub fn span(&self) -> &Span {
        &(self.0).0
    }
}

impl Borrow<SearchSpan> for InTreeSpan {
    fn borrow(&self) -> &SearchSpan {
        &self.0
    }
}

impl PartialEq<InTreeSpan> for InTreeSpan {
    fn eq(&self, other: &InTreeSpan) -> bool {
        self.span().eq(other.span())
    }
}

impl Eq for InTreeSpan { }

impl Ord for InTreeSpan {
    fn cmp(&self, other: &InTreeSpan) -> Ordering {
        self.span().cmp(other.span())
    }
}

impl PartialOrd<InTreeSpan> for InTreeSpan {
    fn partial_cmp(&self, other: &InTreeSpan) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
pub struct SearchSpan(Span, SpanKind);

impl SearchSpan {
    pub fn range(span: &Span) -> Range<SearchSpan> {
        SearchSpan(span.clone(), SpanKind::Start)..SearchSpan(span.clone(), SpanKind::End)
    }
}

impl PartialEq<SearchSpan> for SearchSpan {
    fn eq(&self, other: &SearchSpan) -> bool {
        match (&self.1, &other.1) {
            // In order to be stored inside the BTreeMap, InTree spans should always
            // equate and compare normally. When we compare start and end search spans,
            // they should also equate normally to satisfy some properties of the BTreeMap.
            // All other cases should not equate so we can properly collect the range.
            // Additional details are in the Ord::cmp implementation.
            (&SpanKind::InTree, &SpanKind::InTree)
            | (&SpanKind::Start, &SpanKind::End)
            | (&SpanKind::End, &SpanKind::Start) => self.0 == other.0,
            _ => false,
        }
    }
}

impl Eq for SearchSpan { }

impl Ord for SearchSpan {
    fn cmp(&self, other: &SearchSpan) -> Ordering {
        if *self == *other {
            return Ordering::Equal
        }

        // Answers the question, "Does Span b wrap Span a?"
        fn is_a_in_b(a: &Span, b: &Span) -> bool {
            !(b.range.row_start > a.range.row_start
                || (b.range.row_start == a.range.row_start && b.range.col_start > a.range.col_start)
                || b.range.row_end < a.range.row_end
                || (b.range.row_end == a.range.row_end && b.range.col_end < a.range.col_end))
        }

        match (self, other) {
            // If we're comparing two in-tree spans, they should be compared normally.
            (&SearchSpan(ref s1, SpanKind::InTree), &SearchSpan(ref s2, SpanKind::InTree)) => s1.cmp(s2),

            // If the in-tree span wraps our start search span, we want to mark it as greater
            // so that it will be "inside" the range relating to the start search span.
            // If the in-tree span wraps our end search span, we want the end search to be
            // marked as greater to show that the in-tree span is "inside" the range.
            (&SearchSpan(ref in_tree, SpanKind::InTree), &SearchSpan(ref search, SpanKind::Start))
            | (&SearchSpan(ref search, SpanKind::End), &SearchSpan(ref in_tree, SpanKind::InTree))
                if is_a_in_b(search, in_tree) => Ordering::Greater,

            // If the in-tree span wraps our end search span, we want to mark it as less
            // so that it will be "inside" the range relative to the end search span.
            // If the in-tree span wraps our start search span, we want the start search to be
            // marked as less to show that the in-tree span is "inside" the range.
            (&SearchSpan(ref in_tree, SpanKind::InTree), &SearchSpan(ref search, SpanKind::End))
            | (&SearchSpan(ref search, SpanKind::Start), &SearchSpan(ref in_tree, SpanKind::InTree))
                if is_a_in_b(search, in_tree) => Ordering::Less,

            // These other cases are where the start and end search spans are not inside
            // the in-tree spans, and so we compare the spans normally in order to guide the
            // BTreeMap search down the proper arms of the tree.
            (&SearchSpan(ref left, _), &SearchSpan(ref right, _)) => left.cmp(right),
        }
    }
}

impl PartialOrd<SearchSpan> for SearchSpan {
    fn partial_cmp(&self, other: &SearchSpan) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
