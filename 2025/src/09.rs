// sheesh

use std::cmp::{max, min};
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Could not read input file");
    let input = input.lines();

    let mut red_tiles: Vec<(isize, isize)> = Vec::new();
    for line in input {
        let mut b = line.split(',');
        let x: isize = b.next().expect("Not enough coords").parse().expect("No");
        let y: isize = b.next().expect("Not enough coords").parse().expect("No");
        red_tiles.push((x, y));
    }
    let num_red_tiles = red_tiles.len();

    let mut rect_sizes: Vec<(isize, (isize, isize), (isize, isize))> = Vec::new();
    let mut largest = 0;

    for fst in 0..num_red_tiles {
        for snd in fst + 1..num_red_tiles {
            let (fx, fy) = red_tiles[fst];
            let (sx, sy) = red_tiles[snd];
            let rect_size = ((sx - fx).abs() + 1) * ((sy - fy).abs() + 1);
            rect_sizes.push((rect_size, (fx, fy), (sx, sy)));
            if rect_size > largest {
                largest = rect_size;
            }
        }
    }
    println!("Part 1: {largest}");

    rect_sizes.sort_by_key(|(s, _, _)| *s);
    rect_sizes.reverse();

    red_tiles.push(red_tiles[0]); // close the loop
    let (first_x, first_y) = red_tiles[0];

    for (sz, (fx, fy), (sx, sy)) in rect_sizes {
        let xmin = min(fx, sx);
        let xmax = max(fx, sx);
        let ymin = min(fy, sy);
        let ymax = max(fy, sy);
        let mut disqualified = false;
        // suffices to watch just one corner as long as you disqualify correctly,
        // and the dq checks seem unavoidable no matter how you bite the cookie, soo
        let mut x_leq = first_x <= xmin;
        let mut y_leq = first_y <= ymin;
        let mut x_traversals = 0;
        let mut y_traversals = 0;

        for (x, y) in &red_tiles {
            if xmin < *x && *x < xmax && ymin < *y && *y < ymax {
                disqualified = true;
                break;
            }
            if (*x <= xmin && !x_leq) || (*x > xmin && x_leq) {
                if *y <= ymin {
                    x_traversals += 1;
                } else if ymin < *y && *y < ymax {
                    disqualified = true;
                    break;
                }
                x_leq = !x_leq;
            }
            if (*y <= ymin && !y_leq) || (*y > ymin && y_leq) {
                if *x <= xmin {
                    y_traversals += 1;
                } else if xmin < *x && *x < xmax {
                    disqualified = true;
                    break;
                }
                y_leq = !y_leq;
            }
        }
        if !disqualified && x_traversals % 2 != 0 && y_traversals % 2 != 0 {
            println!("Part 2: {sz}");
            break;
        }
    }
}
