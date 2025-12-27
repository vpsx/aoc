// WOOOOW this was the most evil thing ever shuengkiuuu
use std::fs;

fn main() {
    // I lopped the top off just to see what we were dealing with...
    let input = fs::read_to_string("a.txt").expect("Could not read file");
    let lines = input.lines();

    let mut total_can = 0;
    let mut total_cannot = 0;

    for line in lines {
        print!("{}", line);
        let mut words = line.split(' ');
        let bounds = words
            .next()
            .expect("aoc")
            .trim_end_matches([':'])
            .split('x');
        let mut max_bd = 0;
        let mut min_bd = 0;
        for b in bounds {
            let b: usize = b.parse().expect("aoc");
            if b > max_bd || max_bd == 0 {
                max_bd = b;
            }
            if b < min_bd || min_bd == 0 {
                min_bd = b;
            }
        }
        let min_sides = min_bd / 3;
        let max_sides = max_bd / 3;
        let total_tiles = min_sides * max_sides;

        let mut total_presents = 0;

        loop {
            let Some(w) = words.next() else { break };
            let n: usize = w.parse().expect("aoc");
            total_presents += n;
        }

        let diff;
        if total_presents > total_tiles {
            diff = total_presents - total_tiles;
        } else {
            diff = total_tiles - total_presents;
        }
        print!("   {total_presents} into {total_tiles}");

        if total_tiles >= total_presents {
            print!(" CAN    by {diff}");
            total_can += 1;
        } else {
            print!(" CANNOT by {diff}");
            total_cannot += 1;
        }
        println!();
    }
    println!("{total_can} CAN, {total_cannot} CANNOT");
}
