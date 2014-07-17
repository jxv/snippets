
#[deriving(PartialEq, Eq)]
enum State {
	O,
	X
}

static grid_w: int = 20;
static grid_h: int = 10;


static grid : [[State,..grid_w],..grid_h] =
	[[X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X]
	,[X,O,O,O,X,O,O,O,O,O,O,O,O,O,O,O,O,O,O,X]
	,[X,O,X,O,X,O,O,X,O,O,O,O,O,O,O,O,O,O,O,X]
	,[X,O,O,X,O,O,X,O,O,O,O,O,O,O,O,O,O,O,O,X]
	,[X,O,O,O,O,X,O,O,O,O,O,O,O,O,O,O,O,O,O,X]
	,[X,O,X,X,X,O,O,X,X,O,O,O,O,O,O,O,O,O,O,X]
	,[X,O,O,X,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,X]
	,[X,O,O,X,O,O,X,O,O,O,O,O,O,O,O,O,O,O,O,X]
	,[X,O,O,O,O,O,X,O,O,O,O,O,O,O,O,O,O,O,O,X]
	,[X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X,X]
	];

#[deriving(PartialEq, Eq)]
struct Vec2 {
	x: int,
	y: int
}

static start: Vec2 = Vec2 { x: 18, y: 1 }; 
static goal:  Vec2 = Vec2 { x: 3, y: 2 };

/*
fn print_grid() {
	for y in range(0, grid.len()) {
		for x in range(0, grid[y].len()) {
			print(format!("{:c}", if grid[y][x] == X {'X'} else {' '}))
		}
		print("\n")
	}
}
*/

fn print_grid_and_path(path: &[Vec2]) {
	let mut g: [[char,..grid_w],..grid_h] = [[' ',..grid_w],..grid_h];
	for y in range(0, grid.len()) {
		for x in range(0, grid[y].len()) {
			if grid[y][x] == X {
				g[y][x] = 'X'
			}
		}
	}
	for i in range(0, path.len()) {
		let v = path[i];
		g[v.y][v.x] = '.'
	}
	g[start.y][start.x] = 's';
	g[goal.y][goal.x] = 'g';
	for y in range(0, grid.len()) {
		for x in range(0, grid[y].len()) {
			print!("{:c}", g[y][x])
		}
		print!("\n")
	}
}

fn valid_pos(v: Vec2) -> bool {
	if (v.x < 0 || v.x >= grid_w) {
		return false
	}
	if (v.y < 0 || v.y >= grid_h) {
		return false
	}
	return grid[v.y][v.x] == O
}

fn neighbors(v: Vec2) -> &[Vec2] {
	let mut ns: &[Vec2] = &mut [];
	
	let v_ = Vec2 {x:v.x-1, y:v.y};
	if valid_pos(v_) {
		ns.push(v_)
	}
	
	let v_ = Vec2 {x:v.x+1, y:v.y};
	if valid_pos(v_) {
		ns.push(v_)
	}
	
	let v_ = Vec2 {x:v.x, y:v.y-1};
	if valid_pos(v_) {
		ns.push(v_)
	}
	
	let v_ = Vec2 {x:v.x, y:v.y+1};
	if valid_pos(v_) {
		ns.push(v_)
	}
	
	ns
}

#[deriving(PartialEq, Eq)]
struct Score {
	total: int,
	step: int
}

impl PartialOrd for Score {
	fn lt(&self, other: &Score) -> bool {
		self.total < other.total ||
		(self.total == other.total && self.step < other.step)
	}
	fn le(&self, other: &Score) -> bool {
		self.total <= other.total ||
		(self.total == other.total && self.step <= other.step)
	}
	fn gt(&self, other: &Score) -> bool {
		self.total > other.total ||
		(self.total == other.total && self.step > other.step)
	}
	fn ge(&self, other: &Score) -> bool {
		self.total >= other.total ||
		(self.total == other.total && self.step >= other.step)
	}
}

fn abs(v: int) -> int {
	if v < 0 { -v } else { v }
}

fn hueristic(p0: Vec2, p1: Vec2) -> int {
	abs(p0.x - p1.x) + abs(p0.y - p1.y)
}

fn a_star() -> &[Vec2] {
	let mut visited: [[Option<(Vec2, Score)>,..grid_w],..grid_h] = [[None,..grid_w],..grid_h];
	let mut queue: &[(Vec2, Score)] = &mut [];
	
	queue.push((start, Score { total: hueristic(start, goal), step: 0}));
	
	while queue.len() > 0 {
		let (v,s) = queue[0];
		queue.remove(0);
		
		let mut ns = neighbors(v);
		
		let mut i: uint = 0;

		while i < ns.len() {
			let n = ns[i];
			let s__ = Score { total: hueristic(n, v) + s.step + 1, step: s.step + 1 };

			match visited[n.y][n.x] {
				Some ((_,s_)) =>  {
					if s__ < s_ {
						queue.push((n,s__));
						visited[n.y][n.x] = Some((v,s__));
					} else {
						ns.remove(i);
						i -= 1;
					}
				},
				None => {
					queue.push((n,s__));
					visited[n.y][n.x] = Some((v,s__));
				}
			}
			i += 1;
		}
	}

	let mut path: &[Vec2] = &mut [];
	let mut v: Vec2 = goal;
	while v != start {
		path.push(v);
		match visited[v.y][v.x] {
			Some ((v_,_)) => v = v_,
			None => return &mut []
		}
	}
	path
}

fn main() {
	print_grid_and_path(a_star())
}

