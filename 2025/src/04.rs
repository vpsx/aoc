// Problem - easy. Rust - hard!
use std::fs;

const PART_1: bool = false;

fn main() {
    let input = fs::read_to_string("src/input.txt").expect("Could not read input file");
    let rows = input.lines();

    let mut map: Vec<Vec<u8>> = Vec::new();

    // Negative indices make Rust extremely angry -> make code *extremely* ugly.
    // Not sure what the idiomatic Rust way would be, but for this problem, which
    // is rectangular and finite, it is not too disgusting to just add a border...
    for row in rows {
        let mut map_row: Vec<u8> = Vec::new();
        map_row.push(0);
        for col in row.chars() {
            if col == '@' {
                map_row.push(1);
            } else {
                map_row.push(0);
            }
        }
        map_row.push(0);
        map.push(map_row);
    }
    // ...what is disgusting is how I've added it (:
    let cs = map[0].len();
    // You could do map.push(vec![0;1]) before the for-loop, then reassign map[0]
    // here to [0;cs], but what you've avoided is not pushing over a whole
    // 2d-array-sized chunk of memory, just pushing over a bunch (`rows` length)
    // of (ptr,len,capacity)s. meh. (unless map reallocates lul?!)
    map.insert(0, vec![0; cs]);
    map.push(vec![0; cs]);
    let rs = map.len();

    let mut forklift_accessible = 1;
    let mut forklift_removed = 0;

    while forklift_accessible > 0 {
        forklift_accessible = 0;
        let mut next_map: Vec<Vec<u8>> = map.clone();

        for r in 1..rs - 1 {
            for c in 1..cs - 1 {
                if map[r][c] == 0 {
                    next_map[r][c] = 0;
                    continue;
                };

                let mut adj_rolls = 0;
                for x in [r - 1, r, r + 1] {
                    for y in [c - 1, c, c + 1] {
                        if x == r && y == c {
                            continue;
                        };
                        if map[x][y] == 1 {
                            adj_rolls += 1;
                        }
                    }
                }
                if adj_rolls < 4 {
                    next_map[r][c] = 0;
                    forklift_accessible += 1;
                    forklift_removed += 1;
                } else {
                    next_map[r][c] = 1;
                }
            }
        }
        map = next_map;
        if PART_1 {
            println!("Forklift accessible rolls this round: {forklift_accessible}");
            break;
        };
    }
    println!("Forklift removed: {forklift_removed}");
}
