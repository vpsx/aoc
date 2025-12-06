use std::fs;

struct RangeMarker {
    number: usize,
    fresh_from_here: bool,
}

fn add_range(new_range: (usize, usize), range_map: Vec<RangeMarker>) -> Vec<RangeMarker> {
    let mut new_map: Vec<RangeMarker> = Vec::new();

    let (start, end) = new_range;
    let mut inserted_start = false;
    let mut inserted_end = false;
    let mut prev_was_fresh = false;

    for marker in range_map {
        if inserted_end {
            new_map.push(marker);
            continue;
        }
        if !inserted_start {
            if start <= marker.number {
                if !prev_was_fresh {
                    new_map.push(RangeMarker {
                        number: start,
                        fresh_from_here: true,
                    });
                }
                inserted_start = true;
            } else if start > marker.number {
                prev_was_fresh = marker.fresh_from_here;
                new_map.push(marker);
                continue;
            }
        }
        if inserted_start {
            // Range is inclusive, so the first spoiled number is end + 1.
            if end + 1 < marker.number {
                if marker.fresh_from_here {
                    // tipo we were un-spoiling a spoiled pumpkin patch but dnf
                    new_map.push(RangeMarker {
                        number: end + 1,
                        fresh_from_here: false,
                    });
                    inserted_end = true;
                    new_map.push(marker);
                } else {
                    // some other range marked this bit fresh & trumps you
                    inserted_end = true;
                    new_map.push(marker);
                }
            } else if end + 1 == marker.number {
                if marker.fresh_from_here {
                    // you have joined two fresh realms. woohoo ride forklifts break walls
                    inserted_end = true;
                } else {
                    new_map.push(marker);
                    inserted_end = true;
                }
            } else if end + 1 > marker.number {
                // Delete this marker!!! it's now FRESH!!! weee!!! keep going...
                continue;
            }
        }
    }

    if !inserted_start {
        new_map.push(RangeMarker {
            number: start,
            fresh_from_here: true,
        });
    }
    if !inserted_end {
        new_map.push(RangeMarker {
            number: end + 1,
            fresh_from_here: false,
        });
    }

    new_map
}

fn count_fresh(range_map: Vec<RangeMarker>) -> usize {
    let mut fresh = 0;
    let mut count_from = 0;

    for marker in range_map {
        if marker.fresh_from_here {
            count_from = marker.number;
        } else {
            fresh += marker.number - count_from;
        }
    }
    fresh
}

fn main() {
    let input = fs::read_to_string("input.txt").expect("Could not read input file");
    let rows = input.lines();

    let mut range_map: Vec<RangeMarker> = Vec::new();
    let mut ingredient_ids: Vec<usize> = Vec::new();

    for row in rows {
        match row {
            "" => (),
            s => match row.split_once('-') {
                Some((start, end)) => {
                    let start: usize = start.parse().expect("jingle bells");
                    let end: usize = end.parse().expect("jingle bells");
                    range_map = add_range((start, end), range_map);
                }
                None => {
                    ingredient_ids.push(s.parse().expect("jingle all the way"));
                }
            },
        }
    }

    let mut fresh_avbl_ingr_ids = 0;
    for ig in ingredient_ids {
        let mut in_fresh_range = false;
        for marker in &range_map {
            if marker.number >= ig {
                if in_fresh_range {
                    fresh_avbl_ingr_ids += 1;
                }
                break;
            } else {
                in_fresh_range = marker.fresh_from_here;
            }
        }
    }
    println!("Part 1: {fresh_avbl_ingr_ids}");
    let all_fresh_ingr_ids = count_fresh(range_map);
    println!("Part 2: {all_fresh_ingr_ids}");
}
