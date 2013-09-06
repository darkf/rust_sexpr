extern mod std;

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

/// Determines if the input begins a number
#[inline]
fn beginning_is_num(input: &str, i: uint) -> bool {
	let (c, c2) = (input[i] as char, input[i+1] as char);
	if c.is_digit() || c == '.' { true }
	else if c == '-' && (c2.is_digit() || c2 == '.') { true }
	else { false }
}

/// Reads a character after skipping whitespace
fn read_nows(input: &str, start: uint) -> Option<char> {
	input.iter().skip(start).skip_while(|&c| isws(c)).next()
}

/// Reads a numeric literal (integers or floats)
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

/// Reads a string literal
fn read_string(input: &str, start: uint) -> Option<(~Value, uint)> {
	let mut acc = ~"";
	let mut was_escape = false;
	for i in range(start+1, input.len()) {
		match(input[i] as char) {
			'"' if !was_escape =>
				return Some((~Str(acc), i+1)), // got closing quote
			'\\' if !was_escape => {
				// escape codes
				was_escape = true;
				match input[i+1] as char {
					'\\' => acc.push_char('\\'),
					'"' => acc.push_char('"'),
					't' => acc.push_char('\t'),
					'n' => acc.push_char('\n'),
					c => fail!("unknown escape code: \\%c", c)
				}
			}
			c if !was_escape => acc.push_char(c),
			_ => { was_escape = false }
		}
	}

	// we didn't hit the end " so there must be a parse error
	None
}

/// Reads a list
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

/// Reads an atom (symbol/identifier)
fn read_atom(input: &str, start: uint) -> Option<(~Value, uint)> {
	let atom = std::str::from_chars(input.iter().skip(start).take_while(|&c| !isws(c) && c != ')').to_owned_vec());
	let len = atom.len();
	Some((~Atom(atom), start + len))
}

/// Reads any S-expression value
fn read_value(input: &str, start: uint) -> Option<(~Value, uint)> {
	if(start >= input.len()) {
		return None
	}

	match input[start] as char {
		c if isws(c) => read_value(input, start+1), // ignore whitespace
		'(' => read_list(input, start+1),
		')' => None,
		'"' => read_string(input, start),
		_ if beginning_is_num(input, start) => read_number(input, start),
		_ => read_atom(input, start)
	}
}

/// Public function for parsing a value from a string
pub fn from_str(input: &str) -> Option<Value> {
	match read_value(input, 0) {
		Some((v,_)) => Some(*v),
		None => None
	}
}

#[cfg(test)]
mod test {
	use super::*;

	fn parse_some(input: &str, expr: Value) {
		match from_str(input) {
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
		assert_eq!(from_str(""), None);
		parse_some("123", Num(123.0));
		parse_some("-123", Num(-123.0));
		parse_some("()", List(~[]));
		parse_some("3.14159265358", Num(3.14159265358));
		parse_some("(hi)", List(~[Atom(~"hi")]));
		parse_some("(-hi)", List(~[Atom(~"-hi")]));
		parse_some("(-)", List(~[Atom(~"-")]));
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
	fn test_string() {
		parse_some("\"\"", Str(~""));
		parse_some("\"string\"", Str(~"string"));
		parse_some("\"hi\tthere\n\"", Str(~"hi\tthere\n"));
		parse_some("\"a\\\\b\"", Str(~"a\\b"));
		parse_some("(\"hello\" \"world\")", List(~[Str(~"hello"), Str(~"world")]));
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