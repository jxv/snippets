use std::rand;

#[deriving(Eq, ToStr, Clone)]
enum State
{
	Pos,
	Neg
}

type Weight = f32;

#[deriving(Eq, ToStr, Clone)]
struct Particle
{
	state: State,
	weight: Weight
}

fn resample(ps: &[Particle]) -> Particle
{
	assert!(ps.len() > 0);
	
	let mut val: f32 = rand::random();

	// round robin
	while val > 0.0f32 {
		for p in ps.iter() {
			val -= p.weight;
			if val <= 0.0f32 {
				return *p;
			}
		}
	}
	ps[0]
}

fn particle_filter(mut ps: ~[Particle], ctrl: |Particle| -> State, weigh: |Particle| -> Weight) -> ~[Particle] 
{
	// update
	for p in ps.mut_iter() {
		p.state = ctrl(*p);
		p.weight += weigh(*p)
	}

	let mut ps_ = ps.clone();
	let mut total: f32 = 0.0f32;

	// resample
	for p in ps_.mut_iter() {
		*p = resample(ps);
		total += p.weight;
	}

	// normalize
	for p in ps_.mut_iter() {
		p.weight /= total;
	}

	ps_
}

fn uniform_distribution(mut ps: ~[Particle]) -> ~[Particle]
{
	let len = ps.len() as f32;

	for p in ps.mut_iter() {
		p.weight = 1.0f32 / len;
	}
	ps
}

fn print_particles(ps: &[Particle])
{
	print("[");
	for p in ps.iter() {
		print(format!("({:s}:{:f}), ", p.state.to_str(), p.weight))
	}
	print("]\n");
}

fn ctrl(p: Particle) -> State
{
	p.state
}

fn weigh(p: Particle) -> Weight
{
	return match p.state {
		Neg => 0.33f32,
		Pos => 3.0f32
	} * p.weight
}

fn main()
{
	let p = Particle {state: Neg, weight: 0.0f32};
	let mut ps = ~[p, ..10];
	ps = uniform_distribution(ps);
	ps[0].state = Pos;

	print_particles(ps);
	for _ in range(0,10) {
		ps = particle_filter(ps, ctrl, weigh);
		print_particles(ps);
	}
}

