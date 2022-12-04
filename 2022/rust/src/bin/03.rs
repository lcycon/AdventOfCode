use std::collections::HashSet;

use itertools::Itertools;
use lazy_static::lazy_static;

pub fn part_one(input: &str) -> Option<u32> {
    let mut sum = 0;

    for line in input.lines() {
        let common = get_common_char(line);
        let value = get_value(common);
        sum += value;
    }

    Some(sum)
}

pub fn part_two(input: &str) -> Option<u32> {
    let mut sum = 0;

    for mut chunk in &input.lines().chunks(3) {
        let a_set = chunk.next().unwrap().chars().collect::<HashSet<_>>();
        let b_set = chunk.next().unwrap().chars().collect::<HashSet<_>>();
        let c_set = chunk.next().unwrap().chars().collect::<HashSet<_>>();

        let ab = &a_set & &b_set;
        let abc = &ab & &c_set;
        let fin = abc.iter().next().unwrap();

        sum += get_value(*fin);
    }

    Some(sum)
}

fn get_common_char(input: &str) -> char {
    let len = input.len();
    let half = len / 2;

    let (first, second) = input.split_at(half);
    assert_eq!(first.len(), second.len());
    let first_set = first.chars().collect::<HashSet<_>>();
    let second_set = second.chars().collect::<HashSet<_>>();

    let common = first_set.intersection(&second_set).next().unwrap();

    *common
}

lazy_static! {
    static ref CHARS: Vec<char> = {
        let a = 'a'..='z';
        let b = 'A'..='Z';

        vec![a.collect_vec(), b.collect_vec()].concat()
    };
}

fn get_value(c: char) -> u32 {
    CHARS.iter().position(|e| *e == c).unwrap() as u32 + 1
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 3);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 3);
        assert_eq!(part_one(&input), Some(157));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 3);
        assert_eq!(part_two(&input), Some(70));
    }
}
