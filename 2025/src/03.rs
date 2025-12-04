use std::fs;

const NUM_JOLTAGE_DIGITS: u32 = 12; // 2 for part 1, 12 for part 2

fn make_big_joltage(bank_section: &str, ten_power: u32, running_sum: usize) -> usize {
    let len = bank_section.len();
    let ten_power_usize: usize = ten_power.try_into().unwrap();
    let last_usable_index = len - 1 - ten_power_usize;

    let joltages = bank_section.char_indices();
    let mut max_index = 0;
    let mut max_joltage = 0;

    for (index, joltage) in joltages {
        let joltage = joltage.to_digit(10).expect("Could not parse char");
        if joltage > max_joltage && index <= last_usable_index {
            max_index = index;
            max_joltage = joltage;
        }
    }

    let (_cutoff, remaining_usable) = bank_section.split_at(max_index + 1);

    let max_joltage: usize = max_joltage.try_into().unwrap();
    let running_sum: usize = running_sum + max_joltage * 10usize.pow(ten_power);

    if ten_power == 0 {
        return running_sum;
    } else {
        return make_big_joltage(remaining_usable, ten_power - 1, running_sum);
    }
}

fn main() {
    let input = fs::read_to_string("src/input.txt").expect("Could not read input file");
    let banks = input.lines();

    let mut total_output_joltage = 0;

    for bank in banks {
        total_output_joltage += make_big_joltage(bank, NUM_JOLTAGE_DIGITS - 1, 0);
    }
    println!("Total output joltage: {total_output_joltage}");
}
