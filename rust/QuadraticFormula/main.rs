use std::f32::{from_str_hex};

fn quadraticFormula(a: f32, b: f32, c: f32) -> Option<(f32, f32)>
{
	let dis: f32 = (b * b) - (4.0f32 * a * c);
	if dis < 0.0f32 {
		return None;
	}
	let x0: f32 = (-b + dis.sqrt()) / (2.0f32 * a);
	let x1: f32 = (-b - dis.sqrt()) / (2.0f32 * a);
	Some ((x0, x1))
}


fn main()
{
	let args: ~[~str] = std::os::args();
	
	if args.len() != 4 {
		println("error: invalid arguments");
		println(format!("expected: {:s} <f32> <f32> <f32>", args[0]));
		return
	}

	let mut val = [0.0f32, 0.0f32, 0.0f32];
	
	for i in range(0i, 3) {
		let arg: &str = args[i+1];
		match from_str_hex(arg) {
			None => {
				println(format!("error: argument {:d} \"{:s}\"", i+1, arg));
				return
			}
			Some (f) => val[i] = f
		}
	}

	match quadraticFormula(val[0], val[1], val[2]) {
		None => println("unsolvable: negative discriminant"),
		Some ((x0, x1)) => println(format!("({:f}, {:f})", x0, x1))
	}
}

