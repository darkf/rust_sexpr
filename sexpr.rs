#[deriving(Eq)]
#[deriving(Clone)]
pub enum Value {
	List(~[Value]),
	Atom(~str),
	Str(~str),
	Num(float)
}

#[inline]
fn isws(c: char) -> bool {
	c == ' ' || c == '\t' || c == '\n'
}

fn read_nows(input: &str, start: uint) -> Option<char> {
	input.iter().skip(start).skip_while(|&c| isws(c)).next()
}

fn read_number(input: &str, start: uint) -> Option<(~Value, uint)> {
	let end = input.iter().skip(start).position(|c| isws(c) || c == ')');
	let pos = match end {
		Some(end) => start+end,
		None => input.len()
	};
	let acc = input.slice(start, pos);
	match std::float::from_str(acc) {
		Some(f) => Some((~Num(f), pos)),
		None => None
	}
}

fn read_list(input: &str, start: uint) -> Option<(~Value, uint)> {
	let mut list = ~[];
	let mut i = start;
	loop {
		match read_value(input, i) {
			Some((v,new_i)) => {
				list.push(*v);
				i = new_i;
			}
			None if read_nows(input, i) == Some(')') => return Some((~List(list), i)),
			None => return None
		}
	}
}

fn read_atom(input: &str, start: uint) -> Option<(~Value, uint)> {
	let atom = std::str::from_chars(input.iter().skip(start).take_while(|&c| !isws(c) && c != ')').to_owned_vec());
	let len = atom.len();
	Some((~Atom(atom), start + len))
}

fn read_value(input: &str, start: uint) -> Option<(~Value, uint)> {
	if(start >= input.len()) {
		return None
	}

	match input[start] as char {
		c if isws(c) => read_value(input, start+1), // ignore whitespace
		'(' => read_list(input, start+1),
		')' => None,
		c if c.is_digit() || c == '.' => read_number(input, start),
		_ => read_atom(input, start)
	}
}

pub fn parse(input: &str) -> Option<Value> {
	/*do io::with_str_reader(input) |r| {
		//let c = 
		println("blah: " + input + " -> " + read_token(r));
		List(~[])
	}*/
	match read_value(input, 0) {
		Some((v,_)) => Some(*v),
		None => None
	}
}

mod test {
	use super::{parse, Value, Num, List, Atom};

	fn parse_some(input: &'static str, expr: Value) {
		match parse(input) {
			Some(ref p) if p == &expr => {}
			Some(ref p) => {
				if(p != &expr) {
					fail!("Expected %?, not %?, for %?", expr, *p, input)
				}
			}
			None => fail!("Got None for %?", input)
		}
	}

	#[test]
	fn test_sexpr() {
		assert_eq!(parse(""), None);
		parse_some("123", Num(123.0));
		parse_some("()", List(~[]));
		parse_some("3.14159265358", Num(3.14159265358));
		parse_some("(hi)", List(~[Atom(~"hi")]));
		parse_some("(1)", List(~[Num(1.0)]));
		parse_some("(hi there)", List(~[Atom(~"hi"), Atom(~"there")]));
		parse_some("(hi (there))", List(~[Atom(~"hi"), List(~[Atom(~"there")])]));
		parse_some("(hi 123456)", List(~[Atom(~"hi"), Num(123456.0)]));
		parse_some("(())", List(~[List(~[])]));
		parse_some("(hi (there (fellow (human-bot!))))",
			List(~[Atom(~"hi"),
				List(~[Atom(~"there"),
					List(~[Atom(~"fellow"),
						List(~[Atom(~"human-bot!")])])])])
		);
	}

	#[test]
	fn test_whitespace() {
		// spaces
		parse_some("  (  1  )  ", List(~[Num(1.0)]));
		parse_some("  (  (  )  )  )", List(~[List(~[])]));

		// newlines
		parse_some("\n\n( \n\n )\n\n", List(~[]));
		parse_some("\n\n(\n\n123\n\n)\n\n", List(~[Num(123.0)]));
	}
}