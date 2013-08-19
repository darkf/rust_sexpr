extern mod std;
use std::hashmap::HashMap;
use sexpr;

// A very simple LISP-like language
// Globally scoped, no closures

/// Our value types
#[deriving(Clone)]
pub enum LispValue {
	List(~[LispValue]),
	Atom(~str),
	Str(~str),
	Num(float),
	Fn(~[~str], ~LispValue), // args, body
	// XXX: we need the symbol table to be a @-ptr
	// otherwise we can't mutate it. &-ptrs give lifetime
	// headaches, and ~-ptrs give move headaches.
	BIF(~str, ~[~str], extern fn(@mut HashMap<~str, ~LispValue>, ~[~LispValue])->~LispValue) // built-in function (args, closure)
}

// XXX: this is ugly but it won't automatically derive Eq because of the extern fn
impl Eq for LispValue {
	fn eq(&self, other: &LispValue) -> bool {
		match (self.clone(), other.clone()) {
			(BIF(ref x, _, _), BIF(ref y, _, _)) if *x == *y => true,
			(Str(ref x), Str(ref y)) if *x == *y => true,
			(Num(ref x), Num(ref y)) if *x == *y => true,
			(Atom(ref x), Atom(ref y)) if *x == *y => true,
			(List(ref x), List(ref y)) if *x == *y => true,
			(Fn(ref x, ref x2), Fn(ref y, ref y2)) if *x == *y && *x2 == *y2 => true,
			_ => false
		}
	}
}

fn from_sexpr(sexpr: &sexpr::Value) -> ~LispValue {
	match *sexpr {
		sexpr::List(ref v) => ~List(v.map(|x| *from_sexpr(x))),
		sexpr::Num(v) => ~Num(v),
		sexpr::Str(ref v) => ~Str(v.clone()),
		sexpr::Atom(ref v) => ~Atom(v.clone())
	}
}

/// The type of the global symbol table (string to a value mapping).
type SymbolTable = HashMap<~str, ~LispValue>;

/// Returns a value representing the empty list
#[inline]
pub fn nil() -> ~LispValue {
	~List(~[])
}

/// Creates a new symbol table and returns it
pub fn new_symt() -> SymbolTable {
	HashMap::new()
}

/// Binds a symbol in the symbol table. Replaces if it already exists.
pub fn bind(symt: @mut SymbolTable, name: ~str, value: ~LispValue) {
	symt.insert(name, value);
}

/// Look up a symbol in the symbol table. Fails if not found.
pub fn lookup(symt: @mut SymbolTable, name: ~str) -> ~LispValue {
	match symt.find(&name) {
		Some(v) => v.clone(),
		None => fail!("couldn't find symbol: %s", name)
	}
}

/// Identity function
fn id_(_symt: @mut SymbolTable, v: ~[~LispValue]) -> ~LispValue { v[0] }

fn cons_(_symt: @mut SymbolTable, v: ~[~LispValue]) -> ~LispValue {
	~List(~[*v[0].clone(), *v[1].clone()])
}

fn car_(_symt: @mut SymbolTable, v: ~[~LispValue]) -> ~LispValue {
	match v[0] {
		~List(v_) => {
			~v_[0]
		}
		_ => fail!("car: need a list")
	}
}

fn cdr_(_symt: @mut SymbolTable, v: ~[~LispValue]) -> ~LispValue {
	match v[0] {
		~List(v_) => {
			~v_[1]
		}
		_ => fail!("cdr: need a list")
	}
}

// Print function
fn print_(_symt: @mut SymbolTable, v: ~[~LispValue]) -> ~LispValue {
	match v[0] {
		~Str(s) => printfln!("%s", s),
		_ => fail!("print takes an str")
	}
	nil()
}

/// Initializes standard library functions
pub fn init_std(symt: @mut SymbolTable) {
	bind(symt, ~"id", ~BIF(~"id", ~[~"x"], id_));
	bind(symt, ~"print", ~BIF(~"print", ~[~"msg"], print_));
	bind(symt, ~"cons", ~BIF(~"cons", ~[~"x", ~"y"], cons_));
	bind(symt, ~"car", ~BIF(~"car", ~[~"x"], car_));
	bind(symt, ~"cdr", ~BIF(~"cdr", ~[~"x"], cdr_));
}

fn apply(symt: @mut SymbolTable, f: ~LispValue, args: ~[~LispValue]) -> ~LispValue {
	match *f {
		BIF(name, fnargs, bif) => {
			// apply built-in function
			if fnargs.len() != args.len() {
				fail!("function '%s' requires %u arguments, but it received %u arguments",
					name, fnargs.len(), args.len())
			}

			bif(symt, args)
		}

		_ => fail!("apply: need function")
	}
}

/// Evaluates an s-expression and returns a value.
pub fn eval(symt: @mut SymbolTable, input: sexpr::Value) -> ~LispValue {
	match input {
		sexpr::List(v) => {
			if(v.len() == 0) {
				fail!("eval given empty list")
			}

			// XXX: If we don't clone, the `match` partially moves v,
			// so we can't use it in the match arms.
			let v_ = v.clone();

			// evaluate a list as a function call
			match v[0] {
				sexpr::Atom(sym) => {
					let f = lookup(symt, sym);
					let args = v_.slice(1, v_.len());
					let xargs = args.map(|x| eval(symt, x.clone())); // eval'd args
					apply(symt, f, xargs)
				}
				_ => fail!("function calls take an atom"),
			}

		}
		_ => from_sexpr(&input) // return non-list values as they are
	}
}

#[cfg(test)]
mod test {
	use super::*;
	use sexpr;
	use sexpr::from_str;

	fn read(input: &str) -> sexpr::Value {
		from_str(input).unwrap()
	}

	#[test]
	fn test_eval() {
		let symt = @mut new_symt();
		init_std(symt);
		assert_eq!(eval(symt, read("123")), ~Num(123.0));
		assert_eq!(eval(symt, read("(id 123)")), ~Num(123.0));
		assert_eq!(eval(symt, read("(id (id (id 123)))")), ~Num(123.0));
		// should fail: assert_eq!(eval(&mut symt, read("(1 2 3)")), ~List(~[Num(1.0), Num(2.0), Num(3.0)]));
	}

	#[test]
	fn test_cons() {
		let symt = @mut new_symt();
		init_std(symt);
		assert_eq!(eval(symt, read("(cons 1 2)")), ~List(~[Num(1.0), Num(2.0)]));
		assert_eq!(eval(symt, read("(cons 1 (cons 2 3))")), ~List(~[Num(1.0), 
			List(~[Num(2.0), Num(3.0)])]));
	}

	#[test]
	fn test_car() {
		let symt = @mut new_symt();
		init_std(symt);
		assert_eq!(eval(symt, read("(car (cons 1 2))")), ~Num(1.0));
	}

	#[test]
	fn test_cdr() {
		let symt = @mut new_symt();
		init_std(symt);
		assert_eq!(eval(symt, read("(cdr (cons 1 2))")), ~Num(2.0));
		assert_eq!(eval(symt, read("(cdr (cons 1 (cons 2 3)))")), ~List(~[Num(2.0), Num(3.0)]));
	}
}