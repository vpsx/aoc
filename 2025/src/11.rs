use std::collections::{HashMap, VecDeque};
use std::fs;

fn main() {
    let input = fs::read_to_string("input.txt").expect("Could not read file");
    let lines = input.lines();

    let mut graph = HashMap::new();
    for line in lines {
        let (name, outs) = line.split_once(':').expect("No");
        let outs: Vec<&str> = outs.trim_start().split(' ').collect();
        graph.insert(
            name,
            Dev {
                children: Vec::from(outs),
                inpaths: HashMap::new(),
            },
        );
    }
    graph.insert(
        "out",
        Dev {
            children: Vec::new(),
            inpaths: HashMap::new(),
        },
    );

    let you_to_out = paths(graph.clone(), "you", "out");
    println!("      Part 1: {you_to_out}");

    let svr_to_fft = paths(graph.clone(), "svr", "fft");
    let fft_to_dac = paths(graph.clone(), "fft", "dac");
    let dac_to_out = paths(graph.clone(), "dac", "out");
    let svr_fft_dac_out = svr_to_fft * fft_to_dac * dac_to_out;

    let svr_to_dac = paths(graph.clone(), "svr", "dac");
    let dac_to_fft = paths(graph.clone(), "dac", "fft");
    let fft_to_out = paths(graph.clone(), "fft", "out");
    let svr_dac_fft_out = svr_to_dac * dac_to_fft * fft_to_out;

    let svr_to_out_w_dac_and_fft = svr_fft_dac_out + svr_dac_fft_out;
    println!("      Part 2: {svr_to_out_w_dac_and_fft}");

    let svr_to_out = paths(graph.clone(), "svr", "out");
    println!("Just for fun: {svr_to_out}");
}

#[derive(Clone)]
struct Dev<'a> {
    children: Vec<&'a str>,
    inpaths: HashMap<&'a str, usize>,
}

fn paths<'a>(mut graph: HashMap<&'a str, Dev<'a>>, src: &'a str, dst: &str) -> usize {
    graph
        .entry(src)
        .and_modify(|d| d.inpaths = HashMap::from([(src, 1)]));
    let mut queue: VecDeque<&str> = VecDeque::from([src]);

    while !queue.is_empty() {
        let curr_name = queue.pop_front().expect("No path from src to dst");
        let curr = graph
            .get(curr_name)
            .expect("Could not find device in graph");
        let curr_children = curr.children.clone();
        let mut paths_to_curr = 0;
        for (_, n) in &curr.inpaths {
            paths_to_curr += n;
        }

        for next_name in &curr_children {
            if let Some(existing_backref) = graph[next_name].inpaths.get(curr_name) {
                if *existing_backref == paths_to_curr {
                    continue;
                }
            }
            graph.entry(next_name).and_modify(|d| {
                d.inpaths.insert(curr_name, paths_to_curr);
            });
            queue.push_back(next_name);
        }
    }

    let dst = graph.get(dst).expect("Could not find dst in graph");
    let mut paths_to_dst = 0;
    for (_, n) in &dst.inpaths {
        paths_to_dst += n;
    }
    paths_to_dst
}
