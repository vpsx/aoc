// String lights >>> extension cables

use std::collections::HashSet;
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Could not read input file");
    let input = input.lines();
    let mut boxes: Vec<(isize, isize, isize)> = Vec::new();

    for line in input {
        let mut b = line.split(',');
        let x: isize = b.next().expect("Bad coords").parse().expect("No");
        let y: isize = b.next().expect("Bad coords").parse().expect("No");
        let z: isize = b.next().expect("Bad coords").parse().expect("No");
        boxes.push((x, y, z));
    }
    let boxcount = boxes.len();

    let mut dists: Vec<(isize, (isize, isize, isize), (isize, isize, isize))> = Vec::new();

    for fst in 0..boxcount {
        for snd in fst + 1..boxcount {
            let (fx, fy, fz) = boxes[fst];
            let (sx, sy, sz) = boxes[snd];
            let dist = (sx - fx).pow(2) + (sy - fy).pow(2) + (sz - fz).pow(2);
            // f64 doesn't implement Ord buuut we need not bother with sqrt anyway
            dists.push((dist, boxes[fst], boxes[snd]));
        }
    }
    dists.sort_by_key(|(dist, _, _)| *dist); // is this cheating

    let mut circuits: Vec<HashSet<(isize, isize, isize)>> = Vec::new();
    let mut connections_made = 0;

    for dist in dists {
        let (_, fst, snd) = dist;
        let mut stringlighted = false;
        let mut connecting_other_end_to: Option<usize> = None;
        let num_circuits = circuits.len();

        for i in 0..num_circuits {
            let circuit = &circuits[i]; // only clone later if necessary...
            let contains_fst = circuit.contains(&fst);
            let contains_snd = circuit.contains(&snd);

            if let Some(other_circ_idx) = connecting_other_end_to {
                if contains_fst || contains_snd {
                    let mut circuit = circuit.clone();
                    circuit.extend(circuits[other_circ_idx].iter());
                    circuits[i] = circuit;
                    circuits.remove(other_circ_idx);
                    break;
                }
            }

            if contains_fst && contains_snd {
                stringlighted = true;
            } else if contains_fst {
                let mut circuit = circuit.clone();
                circuit.insert(snd);
                circuits[i] = circuit;
                stringlighted = true;
                connecting_other_end_to = Some(i);
            } else if contains_snd {
                let mut circuit = circuit.clone();
                circuit.insert(fst);
                circuits[i] = circuit;
                stringlighted = true;
                connecting_other_end_to = Some(i);
            }
        }
        if !stringlighted {
            let mut new_circuit = HashSet::new();
            new_circuit.insert(fst);
            new_circuit.insert(snd);
            circuits.push(new_circuit);
        }
        connections_made += 1;

        // Part 1
        if connections_made == 1000 {
            let mut big_bois = circuits.clone();
            big_bois.sort_by_key(|hs| hs.len());
            big_bois.reverse();
            let mut prod = 1;
            for i in 0..3 {
                prod *= big_bois[i].len();
            }
            println!("Part 1 multiply largest circuits: {prod}");
        }
        // Part 2
        if circuits.len() == 1 && circuits[0].len() == boxcount {
            println!("Part 2 multiply last conn x-coords: {}", fst.0 * snd.0);
            break;
        }
    }
}
