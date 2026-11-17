// SPDX-License-Identifier: MPL-2.0
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>

//! A number-free JSON value and its canonical (RFC 8785, JCS) serialisation.
//!
//! Plan format v0.1 contains no numbers, so the value type has no number
//! variant: the edge refuses any document that contains one (`MALFORMED`)
//! before it reaches the core. This removes the only hard part of JCS
//! (ECMAScript number serialisation). What remains, and what this module
//! implements, is: objects with members sorted by UTF-16 code units, no
//! insignificant whitespace, and the RFC 8785 string escaping (`"` and `\`
//! escaped; U+0008, U+0009, U+000A, U+000C, U+000D as `\b \t \n \f \r`; every
//! other C0 control as `\u00xx` with lowercase hex; everything else literal).

use std::cmp::Ordering;

/// A JSON value without numbers.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Json {
    /// `null`
    Null,
    /// `true` / `false`
    Bool(bool),
    /// A string.
    Str(String),
    /// An array.
    Arr(Vec<Json>),
    /// An object, members in any order (the serialiser sorts them).
    Obj(Vec<(String, Json)>),
}

impl Json {
    /// Member `key` of an object, if this is an object that has it.
    pub fn get(&self, key: &str) -> Option<&Json> {
        match self {
            Json::Obj(members) => members.iter().find(|(k, _)| k == key).map(|(_, v)| v),
            _ => None,
        }
    }

    /// The string, if this is a string.
    pub fn as_str(&self) -> Option<&str> {
        match self {
            Json::Str(s) => Some(s),
            _ => None,
        }
    }
}

/// Order two member names by UTF-16 code units, as RFC 8785 §3.2.3 requires.
pub fn utf16_cmp(a: &str, b: &str) -> Ordering {
    a.encode_utf16().cmp(b.encode_utf16())
}

/// Append the RFC 8785 serialisation of the string `s` (with quotes).
pub fn write_string(out: &mut String, s: &str) {
    out.push('"');
    for c in s.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\u{8}' => out.push_str("\\b"),
            '\t' => out.push_str("\\t"),
            '\n' => out.push_str("\\n"),
            '\u{c}' => out.push_str("\\f"),
            '\r' => out.push_str("\\r"),
            c if (c as u32) < 0x20 => {
                const HEX: &[u8; 16] = b"0123456789abcdef";
                let v = c as u32;
                out.push_str("\\u00");
                out.push(HEX[(v >> 4) as usize] as char);
                out.push(HEX[(v & 0xf) as usize] as char);
            }
            c => out.push(c),
        }
    }
    out.push('"');
}

fn write_value(out: &mut String, v: &Json) {
    match v {
        Json::Null => out.push_str("null"),
        Json::Bool(true) => out.push_str("true"),
        Json::Bool(false) => out.push_str("false"),
        Json::Str(s) => write_string(out, s),
        Json::Arr(items) => {
            out.push('[');
            for (i, item) in items.iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                write_value(out, item);
            }
            out.push(']');
        }
        Json::Obj(members) => {
            let mut order: Vec<&(String, Json)> = members.iter().collect();
            order.sort_by(|a, b| utf16_cmp(&a.0, &b.0));
            out.push('{');
            for (i, (k, val)) in order.into_iter().enumerate() {
                if i > 0 {
                    out.push(',');
                }
                write_string(out, k);
                out.push(':');
                write_value(out, val);
            }
            out.push('}');
        }
    }
}

/// The canonical serialisation of `v` (no trailing line feed).
pub fn canonical(v: &Json) -> String {
    let mut out = String::new();
    write_value(&mut out, v);
    out
}

/// The on-disk form the estate requires of a `.json` file (JSON-POLICY J-1):
/// the canonical serialisation followed by exactly one line feed.
pub fn j1(v: &Json) -> String {
    let mut s = canonical(v);
    s.push('\n');
    s
}

/// `true` when `bytes` are exactly the J-1 form of `v`.
///
/// Because the edge's parser keeps one member per name, a document with a
/// duplicate member name never equals the J-1 form of what was parsed, so
/// this check also enforces the I-JSON "no duplicate names" rule.
pub fn is_j1(bytes: &[u8], v: &Json) -> bool {
    bytes == j1(v).as_bytes()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn s(x: &str) -> Json {
        Json::Str(x.to_string())
    }

    #[test]
    fn sorts_members_by_utf16() {
        // U+FF61 sorts after U+1F600 in UTF-8/code point order, but before it
        // in UTF-16 (0xFF61 < 0xD83D is false; 0xD83D < 0xFF61 is true).
        let v = Json::Obj(vec![
            ("\u{ff61}".to_string(), s("a")),
            ("\u{1f600}".to_string(), s("b")),
            ("a".to_string(), s("c")),
        ]);
        assert_eq!(
            canonical(&v),
            "{\"a\":\"c\",\"\u{1f600}\":\"b\",\"\u{ff61}\":\"a\"}"
        );
    }

    #[test]
    fn escapes_per_rfc8785() {
        let mut out = String::new();
        write_string(&mut out, "q\"b\\\u{8}\t\n\u{c}\r\u{1}\u{1f}\u{7f}é/");
        assert_eq!(out, "\"q\\\"b\\\\\\b\\t\\n\\f\\r\\u0001\\u001f\u{7f}é/\"");
    }

    #[test]
    fn nested_and_literals() {
        let v = Json::Obj(vec![
            (
                "b".to_string(),
                Json::Arr(vec![Json::Null, Json::Bool(true), Json::Bool(false)]),
            ),
            ("a".to_string(), Json::Obj(vec![])),
        ]);
        assert_eq!(canonical(&v), "{\"a\":{},\"b\":[null,true,false]}");
        assert!(is_j1(b"{\"a\":{},\"b\":[null,true,false]}\n", &v));
        assert!(!is_j1(b"{\"a\":{},\"b\":[null,true,false]}", &v));
        assert!(!is_j1(b"{\"a\": {},\"b\":[null,true,false]}\n", &v));
    }
}
