//! Provides miscellaneous utilities

use std::ops::{Range, RangeFrom, RangeFull, RangeTo};

/// Returns the longest common prefix of a set of strings.
///
/// If no common prefix exists, `None` is returned.
pub fn longest_common_prefix<'a, I>(iter: I) -> Option<&'a str>
        where I: IntoIterator<Item=&'a str> {
    let mut iter = iter.into_iter();

    let mut pfx = iter.next()?;

    for s in iter {
        let n = pfx.chars().zip(s.chars())
            .take_while(|&(a, b)| a == b).count();

        if n == 0 {
            return None;
        } else {
            pfx = &pfx[..n];
        }
    }

    Some(pfx)
}

/// Implemented for built-in range types
// Waiting for stabilization of `std` trait of the same name
pub trait RangeArgument<T> {
    /// Returns the start of range, if present.
    fn start(&self) -> Option<&T> { None }

    /// Returns the end of range, if present.
    fn end(&self) -> Option<&T> { None }
}

impl<T> RangeArgument<T> for Range<T> {
    fn start(&self) -> Option<&T> { Some(&self.start) }

    fn end(&self) -> Option<&T> { Some(&self.end) }
}

impl<T> RangeArgument<T> for RangeFrom<T> {
    fn start(&self) -> Option<&T> { Some(&self.start) }
}

impl<T> RangeArgument<T> for RangeTo<T> {
    fn end(&self) -> Option<&T> { Some(&self.end) }
}

impl<T> RangeArgument<T> for RangeFull {}

#[cfg(test)]
mod test {
    use super::longest_common_prefix;

    #[test]
    fn test_common_prefix() {
        assert_eq!(longest_common_prefix(["foo", "bar"].iter().cloned()), None);
        assert_eq!(longest_common_prefix(["foo", "foobar"].iter().cloned()), Some("foo"));
        assert_eq!(longest_common_prefix(["foobar", "foo"].iter().cloned()), Some("foo"));
        assert_eq!(longest_common_prefix(["alpha", "alpaca", "alto"].iter().cloned()), Some("al"));
    }
}
