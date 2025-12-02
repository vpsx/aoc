use std::fs;

fn main() {
    let input = fs::read_to_string("src/input.txt").expect("Could not read input file");
    let lines = input.lines();

    let mut zero_count_part_one = 0;
    let mut zero_count_part_two = 0;
    let mut current_pos = 50;

    for line in lines {
        let (direction, clicks) = line.split_at(1);

        let mut clicks: i16 = clicks.parse().expect("Could not parse clicks");
        if direction == "L" {
            clicks *= -1;
        } else if direction != "R" {
            panic!("Bad input?");
        }

        let mut full_cycles = clicks / 100;
        if full_cycles < 0 {
            full_cycles *= -1;
        }
        zero_count_part_two += full_cycles;
        clicks %= 100;

        let new_pos = current_pos + clicks;
        // the current_pos != 0 check also takes care of case where clicks == 0 after mod
        let passed_zero = new_pos >= 100 || new_pos <= 0 && current_pos != 0;
        if passed_zero {
            zero_count_part_two += 1;
        }

        current_pos = new_pos % 100;
        // I don't like this but it greatly simplifies the "passed_zero" logic...
        if current_pos < 0 {
            current_pos += 100;
        }

        if current_pos == 0 {
            zero_count_part_one += 1;
        }
    }

    println!("Zero count part one: {zero_count_part_one}");
    println!("Zero count part two: {zero_count_part_two}");
}
