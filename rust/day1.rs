#!/usr/bin/env run-cargo-script
// cargo-deps: anyhow="1.0"
extern crate anyhow;

use std::env;
use std::fs::File;
use std::path::Path;
use std::io::BufReader;
use std::io::prelude::*;
use anyhow::*;

type Result<T,E=anyhow::Error> = std::result::Result<T,E>;

fn read_input<P>(path: P) -> Result<Vec<usize>>
where P: AsRef<Path>{
    let fd = File::open(path)?;
    let mut reader = BufReader::new(fd);
    let mut elves: Vec<usize> = vec![];
    let mut elf: usize = 0;

    for line in reader.lines() {
        match line?.as_str() {
            "" => { elves.push(elf); elf = 0; }
            s  => { elf += s.parse::<usize>()?; }
        }
    }
    elves.sort_by(|a,b| b.cmp(a));
    Ok(elves)
}

fn part1(elves: &[usize]) -> usize {
    elves[0]
}

fn part2(elves: &[usize]) -> usize {
    elves.iter()
        .take(3)
        .sum::<usize>()
}

fn main() -> Result<()> {
    let args = env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        println!("usage: day1.rs <file>");
        anyhow!("file argument required");
    }

    let input = read_input(&args[1])?;
    println!("part 1: {}", part1(&input));
    println!("part 2: {}", part2(&input));

    Ok(())
}
