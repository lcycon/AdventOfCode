use std::collections::HashSet;

pub fn part_one(input: &str) -> Option<u32> {
    let result = find_first_unique_window(input, 4);

    Some(result as u32)
}

pub fn part_two(input: &str) -> Option<u32> {
    let result = find_first_unique_window(input, 14);

    Some(result as u32)
}

fn find_first_unique_window(input: &str, size: usize) -> usize {
    input
        .as_bytes()
        .windows(size)
        .position(|w| w.iter().collect::<HashSet<_>>().len() == w.len())
        .unwrap()
        + size
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 6);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 6);
        assert_eq!(part_one(&input), Some(10));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 6);
        assert_eq!(part_two(&input), Some(29));
    }
}
