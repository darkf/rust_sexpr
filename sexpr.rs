use std::io;

#[deriving(Eq)]
enum Value {
	List(~[Value]),
	Atom(~str),
	Str(~str),
	Num(float)
}

fn read_number(input: &str, start: uint) -> Option<(~Value, uint)> {
	let end = input.iter().skip(start).position(|c| c == ' ' || c == ')');
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
			None if input[i] as char == ')' => return Some((~List(list), i)),
			None => return None
		}
	}
}

fn read_atom(input: &str, start: uint) -> Option<(~Value, uint)> {
	let atom = std::str::from_chars(input.iter().skip(start).take_while(|&c| c != ' ' && c != ')').to_owned_vec());
	let len = atom.len();
	Some((~Atom(atom), start + len))
}

fn read_value(input: &str, start: uint) -> Option<(~Value, uint)> {
	if(start >= input.len() || input.is_whitespace() || input.slice(start, input.len()).is_whitespace()) {
		return None
	}

	match input[start] as char {
		' ' => read_value(input, start+1),
		'(' => read_list(input, start+1),
		')' => None,
		c if c.is_digit() || c == '.' => read_number(input, start),
		c => read_atom(input, start)
	}
}

fn parse(input: &str) -> Option<Value> {
	/*do io::with_str_reader(input) |r| {
		//let c = 
		println("blah: " + input + " -> " + read_token(r));
		List(~[])
	}*/
	match read_value(input, 0) {
		Some((v,_)) => {
			println(fmt!("v: %?", *v));
			Some(*v)
		}
		None => None
	}
}

#[test]
fn test_sexp() {
	assert_eq!(parse(""), None);
	assert_eq!(parse("123"), Some(Num(123.0)));
	assert_eq!(parse("()"), Some(List(~[])));
	assert_eq!(parse("3.14159265358"), Some(Num(3.14159265358)));
	assert_eq!(parse("(hi)"), Some(List(~[Atom(~"hi")])));
	assert_eq!(parse("(1)"), Some(List(~[Num(1.0)])));
	assert_eq!(parse("(hi there)"), Some(List(~[Atom(~"hi"), Atom(~"there")])));
	assert_eq!(parse("(hi (there))"), Some(List(~[Atom(~"hi"), List(~[Atom(~"there")])])));
	assert_eq!(parse("(hi 123456)"), Some(List(~[Atom(~"hi"), Num(123456.0)])));
	assert_eq!(parse("(())"), Some(List(~[List(~[])])));
	assert_eq!(parse("(hi (there (fellow (human-bot!))))"), Some(
		List(~[Atom(~"hi"),
			List(~[Atom(~"there"),
				List(~[Atom(~"fellow"),
					List(~[Atom(~"human-bot!")])])])])
	));
}