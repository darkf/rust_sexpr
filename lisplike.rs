extern mod std;
use std::hashmap::HashMap;
use sexpr;

// A very simple LISP-like language
// Globally scoped, no closures

/// Our value types
#[deriving(Eq)]
#[deriving(Clone)]
pub enum LispValue {
	List(~[LispValue]),
	Atom(~str),
	Str(~str),
	Num(float),
	Fn(~[~str], ~LispValue) // args, body
}

/// The type of the global symbol table (string to a value mapping).
type SymbolTable = HashMap<~str, ~LispValue>;

/// Creates a new symbol table and returns it
fn new_symt() -> SymbolTable {
	HashMap::new()
}

/// Binds a symbol in the symbol table. Replaces if it already exists.
fn bind(symt: &mut SymbolTable, name: ~str, value: ~LispValue) {
	symt.insert(name, value);
}

/// Look up a symbol in the symbol table. Fails if not found.
fn lookup(symt: &SymbolTable, name: ~str) -> ~LispValue {
	match symt.find(&name) {
		Some(v) => v.clone(),
		None => fail!("couldn't find symbol: %s", name)
	}
}
