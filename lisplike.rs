extern mod std;
use std::hashmap::HashMap;
use std::to_str::ToStr;
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
	Fn(~[~str], ~sexpr::Value), // args, body
	BIF(~str, int, ~[~str], extern fn(@mut HashMap<~str, ~LispValue>, ~[~LispValue])->~LispValue) // built-in function (args, closure)
}

// XXX: this is ugly but it won't automatically derive Eq because of the extern fn
impl Eq for LispValue {
	fn eq(&self, other: &LispValue) -> bool {
		match (self.clone(), other.clone()) {
			(BIF(ref x, _, _, _), BIF(ref y, _, _, _)) if *x == *y => true,
			(Str(ref x), Str(ref y)) if *x == *y => true,
			(Num(ref x), Num(ref y)) if *x == *y => true,
			(Atom(ref x), Atom(ref y)) if *x == *y => true,
			(List(ref x), List(ref y)) if *x == *y => true,
			(Fn(ref x, ref x2), Fn(ref y, ref y2)) if *x == *y && *x2 == *y2 => true,
			_ => false
		}
	}
}

impl LispValue {
	/// Coerces this Lisp value to a native boolean. Empty lists (nil) are falsey,
	/// everything else is truthy.
	fn as_bool(&self) -> bool {
		match *self {
			List([]) => false, // nil
			_ => true
		}
	}
}

impl ToStr for LispValue {
	fn to_str(&self) -> ~str {
		match *self {
			Atom(ref s) => s.clone(),
			Str(ref s) => s.clone(),
			Num(ref f) => f.to_str(),
			Fn(ref args, _) => fmt!("<fn(%u)>", args.len()),
			BIF(ref name, ref arity, _, _) => fmt!("<fn %s(%i)>", name.clone(), *arity),
			List(ref v) => {
				let values: ~[~str] = v.iter().map(|x: &LispValue| x.to_str()).collect();
				fmt!("(%s)", values.connect(" "))
			}
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
	match v {
		[~a, ~b, .._] => ~List(~[a, b]),
		_ => fail!("cons: requires two arguments")
	}
}

fn car_(_symt: @mut SymbolTable, v: ~[~LispValue]) -> ~LispValue {
	match v[0] {
		~List(v_) => ~v_[0],
		_ => fail!("car: need a list")
	}
}

fn cdr_(_symt: @mut SymbolTable, v: ~[~LispValue]) -> ~LispValue {
	match v[0] {
		~List(v_) => ~v_[1],
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

// There are several bugs in the macroing system (#8853, or, #8852 and #8851)
// that prevent us from making these functions macros. In addition, we can't
// return a closure because the type needs to be `extern "Rust" fn`, which
// closures aren't. So we have a little bit of code duplication.

fn plus_(_symt: @mut SymbolTable, v: ~[~LispValue]) -> ~LispValue {
	if(v.len() == 0) { fail!("+ needs operands"); }
	else if(v.len() == 1) {
		return v[0];
	}

	let add = |acc: ~LispValue, b: &~LispValue| {
		match (*acc, b) {
			(Num(ref x), &~Num(ref y)) => ~Num(x.clone() + y.clone()),
			(Str(ref x), &~Str(ref y)) => ~Str(x.clone() + y.clone()),
			_ => fail!("invalid operands to +")
		}
	};

	v.iter().skip(1).fold(v[0].clone(), add)
}

fn minus_(_symt: @mut SymbolTable, v: ~[~LispValue]) -> ~LispValue {
	if(v.len() == 0) { fail!("- needs operands"); }
	else if(v.len() == 1) {
		return v[0];
	}

	let sub = |acc: ~LispValue, b: &~LispValue| {
		match (*acc, b) {
			(Num(ref x), &~Num(ref y)) => ~Num(x.clone() - y.clone()),
			_ => fail!("invalid operands to -")
		}
	};

	v.iter().skip(1).fold(v[0].clone(), sub)
}

fn mul_(_symt: @mut SymbolTable, v: ~[~LispValue]) -> ~LispValue {
	if(v.len() == 0) { fail!("* needs operands"); }
	else if(v.len() == 1) {
		return v[0];
	}

	let mul = |acc: ~LispValue, b: &~LispValue| {
		match (*acc, b) {
			(Num(ref x), &~Num(ref y)) => ~Num(x.clone() * y.clone()),
			_ => fail!("invalid operands to *")
		}
	};

	v.iter().skip(1).fold(v[0].clone(), mul)
}

fn div_(_symt: @mut SymbolTable, v: ~[~LispValue]) -> ~LispValue {
	if(v.len() == 0) { fail!("/ needs operands"); }
	else if(v.len() == 1) {
		return v[0];
	}

	let div = |acc: ~LispValue, b: &~LispValue| {
		match (*acc, b) {
			(Num(ref x), &~Num(ref y)) => ~Num(x.clone() / y.clone()),
			_ => fail!("invalid operands to /")
		}
	};

	v.iter().skip(1).fold(v[0].clone(), div)
}

fn equals_(_symt: @mut SymbolTable, v: ~[~LispValue]) -> ~LispValue {
	match v {
		[a, b] => {
			if a == b { ~Num(1f) }
			else { nil() }
		}
		_ => fail!("invalid operands to =")
	}
}

/// Initializes standard library functions
pub fn init_std(symt: @mut SymbolTable) {
	bind(symt, ~"id", ~BIF(~"id", 1, ~[~"x"], id_));
	bind(symt, ~"print", ~BIF(~"print", 1, ~[~"msg"], print_));
	bind(symt, ~"cons", ~BIF(~"cons", 2, ~[~"x", ~"y"], cons_));
	bind(symt, ~"car", ~BIF(~"car", 1, ~[~"x"], car_));
	bind(symt, ~"cdr", ~BIF(~"cdr", 1, ~[~"x"], cdr_));
	bind(symt, ~"+", ~BIF(~"+", -1, ~[], plus_));
	bind(symt, ~"*", ~BIF(~"*", -1, ~[], mul_));
	bind(symt, ~"-", ~BIF(~"-", -1, ~[], minus_));
	bind(symt, ~"/", ~BIF(~"/", -1, ~[], div_));
	bind(symt, ~"=", ~BIF(~"=", 2, ~[~"x", ~"y"], equals_));

	bind(symt, ~"true", ~Num(1f));
	bind(symt, ~"nil", nil());
}

fn apply(symt: @mut SymbolTable, f: ~LispValue, args: ~[~LispValue]) -> ~LispValue {
	match *f {
		BIF(name, arity, fnargs, bif) => {
			// apply built-in function
			if arity > 0 && fnargs.len() as int != arity {
				fail!("function '%s' requires %d arguments, but it received %u arguments",
					name, arity, args.len())
			}

			bif(symt, args)
		}

		Fn(fnargs, body) => {
			// apply a defined function
			if args.len() != fnargs.len() {
				fail!("function requires %u arguments, but it received %u arguments",
					fnargs.len(), args.len())
			}

			// bind its arguments in the environemnt and evaluate its body
			for (name,value) in fnargs.iter().zip(args.iter()) {
				bind(symt, name.clone(), value.clone());
			}

			eval(symt, *body)
		}

		v => fail!("apply: need function, received %?", v)
	}
}

/// Evaluates an s-expression and returns a value.
pub fn eval(symt: @mut SymbolTable, input: sexpr::Value) -> ~LispValue {
	match input {
		sexpr::List(v) => {
			if(v.len() == 0) {
				fail!("eval given empty list")
			}

			// evaluate a list as a function call
			match v {
				[sexpr::Atom(~"quote"), arg] => from_sexpr(&arg),
				[sexpr::Atom(~"def"), name, value] => {
					// bind a value to an identifier
					let ident = match name {
						sexpr::Atom(s) => s,
						sexpr::Str(s) => s,
						_ => fail!("def requires an atom or a string")
					};

					bind(symt, ident, eval(symt, value));
					nil()
				},
				[sexpr::Atom(~"cond"), ..conds] => {
					//let conds = conds.iter().map(|x: &sexpr::Value| from_sexpr(x));
					for cond in conds.iter() {
						match *cond {
							sexpr::List([ref c, ref e]) => {
								if eval(symt, c.clone()).as_bool() {
									return eval(symt, e.clone())
								}
							}
							_ => fail!("cond: need list of (condition expression)")
						}
					}
					nil()
				}
				[sexpr::Atom(~"fn"), sexpr::List(args), body] => {
					// construct a function
					let args_ = args.iter().map(|x| {
						match x {
							&sexpr::Atom(ref s) => s.clone(),
							_ => fail!("fn: arguments need to be atoms")
						}
					}).collect();
					~Fn(args_, ~body)
				}
				[ref fnval, ..args] => {
					let f = eval(symt, fnval.clone());
					let xargs = args.map(|x| eval(symt, x.clone())); // eval'd args
					apply(symt, f, xargs)
				}
				_ => fail!("eval: requires a variable or an application"),
			}

		}
		sexpr::Atom(v) => {
			// variable
			lookup(symt, v)
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
	fn test_str() {
		let symt = @mut new_symt();
		init_std(symt);
		assert_eq!(eval(symt, read("(id \"hi\")")), ~Str(~"hi"));
		assert_eq!(eval(symt, read("(car (cons \"a\" \"b\"))")), ~Str(~"a"));

		// string concatenation
		assert_eq!(eval(symt, read("(+ \"hi\" \" there\")")), ~Str(~"hi there"));
		assert_eq!(eval(symt, read("(+ \"hi\" \" there\" \" variadic\")")), ~Str(~"hi there variadic"));
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

	#[test]
	fn test_arithmetic() {
		let symt = @mut new_symt();
		init_std(symt);
		assert_eq!(eval(symt, read("(+ 1 3)")), ~Num(4.0));
		assert_eq!(eval(symt, read("(+ 1.5 3)")), ~Num(4.5));
		assert_eq!(eval(symt, read("(+ 5 -3)")), ~Num(2.0));

		assert_eq!(eval(symt, read("(- 5 3)")), ~Num(2.0));
		assert_eq!(eval(symt, read("(- 3 5)")), ~Num(-2.0));
		assert_eq!(eval(symt, read("(- 5 -3)")), ~Num(8.0));

		assert_eq!(eval(symt, read("(* 2 5)")), ~Num(10.0));
		assert_eq!(eval(symt, read("(* 2 -5)")), ~Num(-10.0));

		assert_eq!(eval(symt, read("(/ 10 2)")), ~Num(5.0));
		assert_eq!(eval(symt, read("(/ 10 -2)")), ~Num(-5.0));

		assert_eq!(eval(symt, read("(+ 6 (+ 1 3))")), ~Num(10f));
		assert_eq!(eval(symt, read("(- 6 (- 3 2))")), ~Num(5f));
		
		assert_eq!(eval(symt, read("(+ 1 (+ 2 3) 4)")), ~Num(10.0));

		assert_eq!(eval(symt, read("(+ 5)")), ~Num(5.0));
		assert_eq!(eval(symt, read("(+ -5)")), ~Num(-5.0));
	}

	#[test]
	fn test_quote() {
		let symt = @mut new_symt();
		init_std(symt);
		assert_eq!(eval(symt, read("(quote 5)")), ~Num(5.0));
		assert_eq!(eval(symt, read("(quote x)")), ~Atom(~"x"));
		assert_eq!(eval(symt, read("(quote (1 2 3))")), ~List(~[Num(1f), Num(2f), Num(3f)]));
		assert_eq!(eval(symt, read("(quote (quote x))")), ~List(~[Atom(~"quote"), Atom(~"x")]));
		assert_eq!(eval(symt, read("(+ (quote 1) 2)")), ~Num(3f));

		//assert_eq!(eval(symt, read("(quote 1 2 3 4 5)")), ~Num(5.0));
	}

	#[test]
	fn test_def() {
		let symt = @mut new_symt();
		init_std(symt);
		eval(symt, read("(def x 5)"));
		eval(symt, read("(def y 10)"));
		assert_eq!(eval(symt, read("x")), ~Num(5f));
		assert_eq!(eval(symt, read("y")), ~Num(10f));
		assert_eq!(eval(symt, read("(+ x y)")), ~Num(15f));
	}

	#[test]
	fn test_fn() {
		let symt = @mut new_symt();
		init_std(symt);
		assert_eq!(eval(symt, read("(fn () ())")), ~Fn(~[], ~sexpr::List(~[])));
		assert_eq!(eval(symt, read("(fn (x) (x))")), ~Fn(~[~"x"], ~sexpr::List(~[sexpr::Atom(~"x")])));

		eval(symt, read("(def f (fn (x) (+ 1 x)))"));
		assert_eq!(eval(symt, read("f")), ~Fn(~[~"x"],
			~sexpr::List(~[sexpr::Atom(~"+"), sexpr::Num(1f), sexpr::Atom(~"x")])));
		assert_eq!(eval(symt, read("(f 5)")), ~Num(6f));
	}

	#[test]
	fn test_apply_fn() {
		let symt = @mut new_symt();
		init_std(symt);
		assert_eq!(eval(symt, read("((fn () 0))")), ~Num(0.0));
		assert_eq!(eval(symt, read("((fn (x) x) 5)")), ~Num(5.0));
	}

	#[test]
	fn test_cond() {
		let symt = @mut new_symt();
		init_std(symt);
		assert_eq!(eval(symt, read("(cond (true 2) (nil 3))")), ~Num(2f));
		assert_eq!(eval(symt, read("(cond (nil 2) (true 3))")), ~Num(3f));
		assert_eq!(eval(symt, read("(cond (nil 2) (true 3) (true 4))")), ~Num(3f));
	}

	#[test]
	fn test_equals() {
		let symt = @mut new_symt();
		init_std(symt);
		assert_eq!(eval(symt, read("(= 1 1)")), ~Num(1f));
		assert_eq!(eval(symt, read("(= 1.0 1)")), ~Num(1f));
		assert_eq!(eval(symt, read("(= 1 2)")), nil());
		assert_eq!(eval(symt, read("(= true 1)")), ~Num(1f));
		assert_eq!(eval(symt, read("(= nil (quote ()))")), ~Num(1f));
		assert_eq!(eval(symt, read("(= nil nil)")), ~Num(1f));
		assert_eq!(eval(symt, read("(= nil true)")), nil());

		assert_eq!(eval(symt, read("(= \"a\" \"a\")")), ~Num(1f));
		assert_eq!(eval(symt, read("(= \"a\" \"b\")")), nil());

		assert_eq!(eval(symt, read("(= (quote (1 2 3)) (quote (1 2 3)))")), ~Num(1f));
	}

	#[test]
	fn test_factorial() {
		let symt = @mut new_symt();
		init_std(symt);
		eval(symt, read("(def fac (fn (n) (cond ((= n 0) 1) (true (* n (fac (- n 1)))))))"));
		assert_eq!(eval(symt, read("(fac 10)")), ~Num(3628800f));
	}
}

#[ignore(test)]
#[main]
fn main() {
	// A simple REPL
	let stdin = std::io::stdin();
	let mut symt = @mut new_symt();
	init_std(symt);

	loop {
		print("> ");
		let line = stdin.read_line();
		match line {
			~".q" => break,
			~".newsym" => {
				// use a fresh symbol table
				symt = @mut new_symt();
				init_std(symt);
				println("ok");
			}
			_ => {
				match sexpr::from_str(line) {
					Some(sexpr) => println(eval(symt, sexpr).to_str()),
					None => println("syntax error")
				}
			}
		}
	}
}