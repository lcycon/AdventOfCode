use std::{collections::HashSet, ops::RangeInclusive, str::FromStr};

use itertools::Itertools;

struct Input(InRange, InRange);

impl FromStr for Input {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (a, b) = s.split_once(',').unwrap();

        Ok(Input(a.parse().unwrap(), b.parse().unwrap()))
    }
}

struct InRange(RangeInclusive<u32>);

impl InRange {
    fn contains(&self, other: &InRange) -> bool {
        self.0.start() <= other.0.start() && self.0.end() >= other.0.end()
    }

    fn overlaps(&self, other: &InRange) -> bool {
        (other.0.start() <= self.0.start() && self.0.start() <= other.0.end())
            || (other.0.start() <= self.0.end() && self.0.end() <= other.0.end())
    }
}

impl FromStr for InRange {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (a, b) = s.split_once('-').unwrap();

        Ok(InRange(a.parse().unwrap()..=b.parse().unwrap()))
    }
}

pub fn part_one(input: &str) -> Option<u32> {
    let pairs: Vec<Input> = input.lines().map(|v| v.parse().unwrap()).collect_vec();
    let mut count = 0;

    for Input(one, two) in pairs {
        if one.contains(&two) || two.contains(&one) {
            count += 1;
        }
    }

    Some(count)
}

pub fn part_two(input: &str) -> Option<u32> {
    let pairs: Vec<Input> = input.lines().map(|v| v.parse().unwrap()).collect_vec();
    let mut count = 0;

    for Input(one, two) in pairs {
        if one.overlaps(&two) || two.overlaps(&one) {
            count += 1;
        }
    }

    Some(count)
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 4);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 4);
        assert_eq!(part_one(&input), Some(2));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 4);
        assert_eq!(part_two(&input), Some(4));
    }
}
