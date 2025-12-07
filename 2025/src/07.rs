use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Could not read input file");
    let lines = input.lines();

    let mut current_beam: Vec<usize> = Vec::new();
    let mut new_beam: Vec<usize> = Vec::new();
    let mut add_next = 0;
    let mut split_count = 0;

    for line in lines {
        if current_beam.len() == 0 {
            current_beam = line.chars().map(|c| if c == 'S' { 1 } else { 0 }).collect();
            continue;
        }
        for (curr, next) in current_beam.into_iter().zip(line.chars()) {
            if next == '^' {
                if curr != 0 {
                    let left = new_beam
                        .pop()
                        .expect("Assumed no splitters on edge of manifold");
                    new_beam.push(left + curr);
                    new_beam.push(0);
                    add_next = curr;
                    split_count += 1;
                } else {
                    new_beam.push(0);
                }
            } else if next == '.' {
                new_beam.push(curr + add_next);
                add_next = 0;
            }
        }
        current_beam = new_beam;
        new_beam = Vec::new();
    }
    let possible_timelines: usize = current_beam.into_iter().sum();

    println!("Part 1 classical split count: {split_count}");
    println!("Part 2 possible timelines: {possible_timelines}");
}
