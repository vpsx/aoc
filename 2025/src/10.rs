// this one was a pollaborative effort)
// I think the intended beta was to either (1) observe that for lights, you
// need press each button at most once, which makes BFS (breadth first? brute
// force?) tractable, so whip up a BFS for part 1; or (2) see part 2 coming
// from miles away and just jump to linear programming.
// I did neither of those...

use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Could not read file");
    let lines = input.lines();

    let mut button_presses_lights = 0;
    let mut button_presses_jolts = 0;

    for line in lines {
        let mut words = line.split(' ');
        let lights_goal = words.next().expect("aoc").trim_matches(['[', ']']);
        let lights_goal: Vec<usize> = lights_goal
            .chars()
            .map(|x| if x == '#' { 1 } else { 0 })
            .collect();
        let machine_length = lights_goal.len();

        let mut buttons: Vec<Vec<usize>> = Vec::new();
        let mut word = words.next().expect("should break on seeing joltages");

        while word.starts_with('(') {
            let indices = word.trim_matches(['(', ')']).split(',');
            let indices: Vec<usize> = indices.map(|c| c.parse().expect("aoc")).collect();
            let mut button: Vec<usize> = Vec::new();
            button.resize(machine_length, 0);
            for i in indices {
                button[i] = 1;
            }
            buttons.push(button);
            word = words.next().expect("should break on seeing joltages");
        }

        let jolts = word.trim_matches(['{', '}']).split(',');
        let jolts_goal: Vec<usize> = jolts.map(|c| c.parse().expect("aoc")).collect();

        let mut aug_mat_lights: Vec<Vec<isize>> = Vec::new();
        let mut aug_mat_jolts: Vec<Vec<isize>> = Vec::new();
        for row_idx in 0..machine_length {
            let mut row_lights: Vec<isize> = Vec::new();
            let mut row_jolts: Vec<isize> = Vec::new();
            for button in &buttons {
                row_lights.push(button[row_idx].try_into().unwrap());
                row_jolts.push(button[row_idx].try_into().unwrap());
            }
            row_lights.push(lights_goal[row_idx].try_into().unwrap());
            row_jolts.push(jolts_goal[row_idx].try_into().unwrap());
            aug_mat_lights.push(row_lights);
            aug_mat_jolts.push(row_jolts);
        }

        button_presses_lights += solve(aug_mat_lights, true);
        button_presses_jolts += solve(aug_mat_jolts, false);
    }
    println!("Part 1 button presses: {button_presses_lights}");
    println!("Part 2 button presses: {button_presses_jolts}");
}

fn solve(mut aug_mat: Vec<Vec<isize>>, lights_mode: bool) -> usize {
    let mut basics: Vec<usize> = Vec::new();
    let mut nonbasics: Vec<usize> = Vec::new();
    let row_length = aug_mat[0].len();
    let goal_idx = row_length - 1;
    let buttons_length = row_length - 1;

    'gaussian_elim: for idx in 0..buttons_length {
        let mut this_idxs_row = None;
        'finding_this_idxs_row: for row in &aug_mat {
            for sub_idx in 0..idx {
                if row[sub_idx] != 0 {
                    // row is already 'in use' for a previous index
                    continue 'finding_this_idxs_row;
                }
            }
            if row[idx] != 0 {
                this_idxs_row = Some(row.clone());
                break 'finding_this_idxs_row;
            }
        }
        let Some(this_idxs_row) = this_idxs_row else {
            nonbasics.push(idx);
            continue 'gaussian_elim;
        };
        basics.push(idx);

        let zeroer = this_idxs_row[idx];
        let mut skipped_this_idxs_row = false;
        'zeroing_other_rows: for row in &mut aug_mat {
            if !skipped_this_idxs_row && *row == this_idxs_row {
                skipped_this_idxs_row = true;
                continue 'zeroing_other_rows;
            } else if row[idx] != 0 {
                if lights_mode {
                    // multiplication forbidden in this world
                    for i in 0..row_length {
                        row[i] = (row[i] - this_idxs_row[i]).abs();
                    }
                } else {
                    let mut zeroed = row[idx];
                    if zeroed % zeroer != 0 {
                        // can't evenly divide; want integers only; multiply instead
                        for i in 0..row_length {
                            row[i] = row[i] * zeroer;
                        }
                        zeroed = row[idx];
                    }
                    let multiplier = -1isize * zeroed / zeroer;
                    let zeroer_row: Vec<isize> =
                        this_idxs_row.iter().map(|x| x * multiplier).collect();
                    for i in 0..row_length {
                        row[i] = row[i] + zeroer_row[i];
                    }
                }
            }
        }
    }

    // This is integer/modular/weird LP; we are... not going to pivot )))))0000
    // We will simply enumerate all possible settings given the non-basic maxes.
    let mut non_basic_maxes: Vec<(usize, usize)> = Vec::new();
    if lights_mode {
        // Modulo means the button presses can still be 1 even if lights goal is 0.
        // Also means that the max presses needed for any one button is 1!
        non_basic_maxes = nonbasics.iter().map(|x| (*x, 1)).collect();
    } else {
        for nb in &nonbasics {
            let mut nbmax: isize = 0;
            for row in &aug_mat {
                if row[*nb] != 0 {
                    let a = row[goal_idx] / row[*nb];
                    if a > nbmax {
                        nbmax = a;
                    }
                }
            }
            if nbmax < 0 {
                nbmax = 0
            };
            let nbmax: usize = nbmax.try_into().unwrap();
            non_basic_maxes.push((*nb, nbmax));
        }
    }

    let mut settings: Vec<Vec<(usize, usize)>> = Vec::new();
    for (idx, lim) in non_basic_maxes {
        if settings.is_empty() {
            settings = (0..=lim).map(|x| Vec::from([(idx, x)])).collect();
        } else {
            let mut new_setting: Vec<Vec<(usize, usize)>> = Vec::new();
            for s in settings {
                let mut new_gamut: Vec<Vec<(usize, usize)>> = (0..=lim)
                    .map(|x| {
                        let mut c = s.clone();
                        c.push((idx, x));
                        c
                    })
                    .collect();
                new_setting.append(&mut new_gamut);
            }
            settings = new_setting;
        }
    }

    if settings.is_empty() {
        let mut solutions: Vec<(usize, usize)> = Vec::new();
        for eq in aug_mat {
            for idx in 0..buttons_length {
                if eq[idx] != 0 {
                    if lights_mode {
                        let sol: usize = eq[goal_idx].try_into().unwrap();
                        solutions.push((idx, sol));
                    } else {
                        if eq[goal_idx] % eq[idx] != 0 {
                            panic!("Unique solution but infeasible (non integer)");
                        }
                        let sol = eq[goal_idx] / eq[idx];
                        if sol < 0 {
                            panic!("Unique solution but infeasible (negative)");
                        }
                        let sol: usize = sol.try_into().unwrap();
                        solutions.push((idx, sol));
                    }
                }
            }
        }
        let sum_button_presses = solutions.iter().fold(0, |acc, (_, n)| acc + n);
        return sum_button_presses;
    } else {
        let mut min_sum_button_presses = 0;
        'plugging: for setting in settings {
            let mut solutions = setting.clone();
            for row in &aug_mat {
                let mut eq = row.clone();
                for (i, a) in &solutions {
                    let a: isize = isize::try_from(*a).expect("Couldn't convert usize to isize");
                    if eq[*i] != 0 {
                        eq[goal_idx] -= eq[*i] * a;
                        eq[*i] = 0;
                    }
                }
                for idx in 0..buttons_length {
                    if eq[idx] != 0 {
                        if lights_mode {
                            let sol: usize = (eq[goal_idx] % 2).abs().try_into().unwrap();
                            solutions.push((idx, sol));
                        } else {
                            if eq[goal_idx] % eq[idx] != 0 {
                                continue 'plugging; // infeasible (non integer)
                            }
                            let sol = eq[goal_idx] / eq[idx];
                            if sol < 0 {
                                continue 'plugging; // infeasible (negative)
                            }
                            let sol: usize = sol.try_into().unwrap();
                            solutions.push((idx, sol));
                        }
                    }
                }
            }
            let solsum = solutions.iter().fold(0, |acc, (_, n)| acc + n);
            if solsum < min_sum_button_presses || min_sum_button_presses == 0 {
                min_sum_button_presses = solsum;
            }
        }
        return min_sum_button_presses;
    }
}
