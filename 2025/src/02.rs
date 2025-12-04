// I hate this

use std::collections::HashSet;

// example input
const INPUT: &str = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124";

//const PRIMES_ISH: [usize; 1] = [2]; // part 1
const PRIMES_ISH: [usize; 6] = [2, 3, 5, 7, 9, 11]; // part 2

fn main() {
    let ranges = INPUT.split(',');

    let mut sum = 0;

    for range in ranges {
        let Some((start, end)) = range.split_once('-') else {
            panic!("Bad range.")
        };
        if end.len() > 11 {
            panic!("We need more primes...")
        };
        let start: usize = start.parse().expect("Could not parse start");
        let end: usize = end.parse().expect("Could not parse end");

        let mut naughty_list = HashSet::new(); // Буу! Необходимый! Or is it

        for repetitions in PRIMES_ISH {
            for unit in 1..end {
                let unit = unit.to_string();
                let invalid = unit.repeat(repetitions);
                let invalid: usize = invalid.parse().expect("Could not parse invalid");

                if invalid < start {
                    continue;
                };
                if invalid > end {
                    break;
                };

                // Naughty invalids are invalids that... look like 222222:
                // they have a length divisible by > 1 prime and consist of a single digit.
                // ... ... ...but is doing those checks (and checking if we're on the smallest
                // divisible prime/rep) actually faster than a HashSet lookup??? I have no idea.
                if naughty_list.contains(&invalid) {
                    continue;
                };
                naughty_list.insert(invalid);
                sum += invalid;
            }
        }
    }

    println!("Sum: {sum}");
}
