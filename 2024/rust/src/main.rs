use std::collections::HashMap;

use clap::Parser;
use lazy_static::lazy_static;

mod days;

type PartFunc = fn(&str) -> u64;

lazy_static! {
    static ref PART_MAP: HashMap<(u8, u8), PartFunc> = HashMap::from([
        ((5u8, 1u8), days::day5::part1 as PartFunc),
        ((5u8, 2u8), days::day5::part2 as PartFunc),
        ((6u8, 1u8), days::day6::part1 as PartFunc),
        ((6u8, 2u8), days::day6::part2 as PartFunc),
        ((7u8, 1u8), days::day7::part1 as PartFunc),
        ((7u8, 2u8), days::day7::part2 as PartFunc),
        ((9u8, 1u8), days::day9::part1 as PartFunc),
        ((9u8, 2u8), days::day9::part2 as PartFunc),
        ((11u8, 1u8), days::day11::part1 as PartFunc),
        ((11u8, 2u8), days::day11::part2 as PartFunc),
    ]);
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    day: u8,
    #[arg(short, long)]
    part: u8,
    #[arg(short, long)]
    example: bool,
}

fn main() {
    let args = Args::parse();

    let part_func = PART_MAP
        .get(&(args.day, args.part))
        .expect("Couldn't find that day/part");

    let example_string = if args.example { "example" } else { "" };

    let input = std::fs::read_to_string(format!("inputs/day{:02}{example_string}.txt", args.day))
        .expect("Couldn't read input file");

    let answer = part_func(&input);

    println!("Answer: {}", answer);
}
