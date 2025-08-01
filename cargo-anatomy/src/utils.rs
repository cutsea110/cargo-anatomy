//! Utilities for analyzing Rust crates and computing package metrics.
use std::io;
use std::panic::Location;

#[track_caller]
pub fn error_with_location<E>(err: E) -> Box<dyn std::error::Error>
where
    E: std::fmt::Display,
{
    let loc = Location::caller();
    Box::new(io::Error::other(format!(
        "{} at {}:{}",
        err,
        loc.file(),
        loc.line()
    )))
}

#[macro_export]
macro_rules! loc_try {
    ($expr:expr) => {
        match $expr {
            Ok(val) => val,
            Err(err) => {
                return Err($crate::error_with_location(err));
            }
        }
    };
}

pub(crate) fn has_test_attr(attrs: &[syn::Attribute]) -> bool {
    attrs.iter().any(|a| {
        if a.path().is_ident("test") {
            true
        } else if a.path().is_ident("cfg") {
            match &a.meta {
                syn::Meta::List(l) => l.tokens.to_string().contains("test"),
                _ => false,
            }
        } else {
            false
        }
    })
}
