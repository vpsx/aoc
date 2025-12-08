use std::fs;
use std::str::Chars;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Could not read input file");
    part1(&input);
    part2(&input);
}

// Tried using closures and got:
//   = note: no two closures, even if identical, have the same type
//   = help: consider boxing your closure and/or using it as a trait object
// It is day 7 of using rust and I have no idea how to do either of those.
// So tried functions; similar deal but the compiler said to use fn pointers.
// Which I think is basically the same idea as boxing closures?
// I guess having named closures was weird anyway (insofar as here they were
// just being used in the spirit of lambdas and were not closure-ing anything)
// (like, named lambdas would be a bit funny).
const ADD: fn(usize, usize) -> usize = |a, b| a + b;
const MUL: fn(usize, usize) -> usize = |a, b| a * b;

fn part1(input: &str) {
    let lines = input.lines();
    let operators = lines.last().expect("No");
    let operators = operators.split_whitespace();
    let mut heads: Vec<(fn(usize, usize) -> usize, usize)> = operators
        .map(|op| if op == "*" { (MUL, 1) } else { (ADD, 0) })
        .collect();

    let lines = input.lines();
    for line in lines {
        if line.starts_with(&['+', '*']) {
            break;
        }
        let new_operands = line.split_whitespace().map(|n| {
            let n: usize = n.parse().expect("Bad operand");
            n
        });

        heads = heads
            .into_iter()
            .zip(new_operands)
            .map(|((op, a), b)| (op, op(a, b)))
            .collect();
    }

    let grand_total: usize = heads.into_iter().map(|(_, x)| x).sum();
    println!("Part 1 grand total: {grand_total}");
}

fn part2(input: &str) {
    let lines = input.lines();
    let mut charcols: Vec<Chars<'_>> = Vec::new();
    for line in lines {
        charcols.push(line.chars());
    }

    let mut unrolled_completely = false;
    let mut curr_op = ADD;
    let mut operands: Vec<usize> = Vec::new();
    let mut grand_total = 0;

    while !unrolled_completely {
        let mut column: String = String::new();
        for col in &mut charcols {
            let col = col.next();
            match col {
                None => unrolled_completely = true,
                Some(x) => column.push(x),
            }
        }

        column = column.trim().to_string();
        if column.is_empty() {
            grand_total += operands.into_iter().reduce(|x, e| curr_op(x, e)).unwrap();
            operands = Vec::new();
            continue;
        } else if column.contains("*") {
            curr_op = MUL;
        } else if column.contains("+") {
            curr_op = ADD;
        }
        operands.push(
            column
                .trim_end_matches(['+', '*'])
                .trim()
                .parse()
                .expect("Bad column"),
        );
    }

    println!("Part 2 grand total: {grand_total}");
}
