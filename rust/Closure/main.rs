
struct Test
{
	my_fn: || -> ()
}

fn main() {
	println!("test");
	let mut x: int = 3;
	let cls = || -> () { x = x+1; println!("{}", x) };
	let test: Test = Test { my_fn: cls };
	cls();
	cls();
	cls();
}
