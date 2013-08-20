`rust_sexpr` is a library for parsing [S-expressions](http://en.wikipedia.org/wiki/S-expression), and contains a simple globally-scoped evaluator for a LISP-like language.

Building
=========

For both the s-expr library *and* lisplike,

    $ rustc --lib lisplike.rc

For just the s-expr library,

    $ rustc --lib sexpr.rs

To run the unit tests, compile with `--test`.


Usage
=========

    extern mod sexpr;
    
    fn main() {
        let value: Option<sexpr::Value> = sexpr::from_str("(1 2 3.5)");
        match value {
        	Some(expr) =>
        	  assert_eq!(expr, sexpr::List(~[sexpr::Num(1.0), sexpr::Num(2.0), sexpr::Num(3.5)])),
        	None =>
        	  fail!("There was an error parsing the S-expression!")
        }
    }