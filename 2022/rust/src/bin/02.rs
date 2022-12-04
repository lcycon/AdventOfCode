use itertools::Itertools;

pub fn part_one(input: &str) -> Option<u32> {
    let rounds = input.lines().collect_vec();

    let mut score = 0;

    for (op, me) in rounds
        .iter()
        .map(|r| (r.chars().next().unwrap(), r.chars().nth(2).unwrap()))
    {
        score += match (op, me) {
            ('A', 'X') | ('B', 'Y') | ('C', 'Z') => cscore(me) + 3,
            ('C', 'X') => cscore(me) + 6,
            ('A', 'Y') => cscore(me) + 6,
            ('B', 'Z') => cscore(me) + 6,
            _ => cscore(me),
        };
    }

    Some(score)
}

pub fn part_two(input: &str) -> Option<u32> {
    let rounds = input.lines().collect_vec();

    let mut score = 0;

    for (op, me) in rounds
        .iter()
        .map(|r| (r.chars().next().unwrap(), r.chars().nth(2).unwrap()))
    {
        score += match (op, me) {
            (_, 'X') => oscore(op, me),
            (_, 'Y') => oscore(op, me) + 3,
            (_, 'Z') => oscore(op, me) + 6,
            _ => 0,
        };
    }

    Some(score)
}

fn cscore(input: char) -> u32 {
    match input {
        'X' => 1,
        'Y' => 2,
        'Z' => 3,
        _ => 0,
    }
}
fn oscore(op: char, me: char) -> u32 {
    match (op, me) {
        ('A', 'X') => 3,
        ('A', 'Y') => 1,
        ('A', 'Z') => 2,
        ('B', 'X') => 1,
        ('B', 'Y') => 2,
        ('B', 'Z') => 3,
        ('C', 'X') => 2,
        ('C', 'Y') => 3,
        ('C', 'Z') => 1,
        _ => 0,
    }
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 2);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 2);
        assert_eq!(part_one(&input), Some(15));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 2);
        assert_eq!(part_two(&input), Some(12));
    }
}
