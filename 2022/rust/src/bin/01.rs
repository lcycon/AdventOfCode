use itertools::Itertools;

pub fn part_one(input: &str) -> Option<u32> {
    let parsed = parse_input(input);

    Some(parsed.into_iter().max().unwrap())
}

pub fn part_two(input: &str) -> Option<u32> {
    let parsed = parse_input(input);

    Some(parsed.into_iter().sorted().rev().take(3).sum())
}

fn parse_input(input: &str) -> Vec<u32> {
    input
        .lines()
        .collect_vec()
        .split(|v| (*v).is_empty())
        .map(|v| v.iter().map(|e| e.parse::<u32>().unwrap()).sum())
        .collect_vec()
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 1);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 1);
        assert_eq!(part_one(&input), Some(24000));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 1);
        assert_eq!(part_two(&input), Some(45000));
    }
}
