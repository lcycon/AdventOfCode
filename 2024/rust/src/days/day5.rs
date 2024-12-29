use std::str::FromStr;

use anyhow::anyhow;

pub fn part1(input: &str) -> u64 {
    let (constraints, updates) = parse_input(input);

    let mut total = 0;

    for update in updates.iter() {
        let correct = is_correctly_ordered(update, &constraints);

        if correct {
            let middle_element_value = update[(update.len() - 1) / 2];
            total += middle_element_value as u64;
        }
    }

    total
}

pub fn part2(input: &str) -> u64 {
    let (constraints, updates) = parse_input(input);

    let mut total = 0;

    for update in updates.iter() {
        let correct = is_correctly_ordered(update, &constraints);

        if correct {
            continue;
        }

        let corrected = correct_order(update, &constraints);
        let middle_element_value = corrected[(corrected.len() - 1) / 2];
        total += middle_element_value as u64;
    }

    total
}

fn is_correctly_ordered(update: &[u16], constraints: &[Constraint]) -> bool {
    let mut correct = true;

    for Constraint(fst, snd) in constraints.iter() {
        // Find the index of `fst` in `update`
        let fst_idx = update.iter().position(|&x| x == *fst);
        // Find the index of `snd` in `update`
        let snd_idx = update.iter().position(|&x| x == *snd);

        // Update the `correct` flag by performing a logical AND (&) with the following conditions:
        // 1. Either the first number is not found in the update (fst_idx.is_none()), OR
        // 2. Either the second number is not found in the update (snd_idx.is_none()), OR
        // 3. If both numbers are found, the first number must appear before the second number
        //    (fst_idx.unwrap() < snd_idx.unwrap())
        correct = correct
            && (fst_idx.is_none() || snd_idx.is_none() || fst_idx.unwrap() < snd_idx.unwrap());
    }

    correct
}

fn correct_order(update: &[u16], constraints: &[Constraint]) -> Vec<u16> {
    let mut numbers = update.to_vec();
    let mut changed = true;

    while changed {
        changed = false;
        for Constraint(fst, snd) in constraints.iter() {
            let fst_idx = numbers.iter().position(|&x| x == *fst);
            let snd_idx = numbers.iter().position(|&x| x == *snd);

            // If both numbers exist and they're in the wrong order
            if let (Some(i), Some(j)) = (fst_idx, snd_idx) {
                if i > j {
                    // Swap the numbers to maintain the correct order
                    numbers.swap(i, j);
                    changed = true;
                }
            }
        }
    }

    numbers
}

struct Constraint(u16, u16);

impl FromStr for Constraint {
    type Err = anyhow::Error;

    // Parses a `Constraint` from a string of the form `47|53`
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut parts = s.split('|');
        let lower = parts
            .next()
            .ok_or(anyhow!("Missing lower bound"))?
            .parse()?;
        let upper = parts
            .next()
            .ok_or(anyhow!("Missing upper bound"))?
            .parse()?;
        Ok(Constraint(lower, upper))
    }
}

fn parse_input(input: &str) -> (Vec<Constraint>, Vec<Vec<u16>>) {
    let mut parts = input.split("\n\n");

    let constraints = parts
        .next()
        .unwrap_or("")
        .lines()
        .filter(|line| !line.is_empty())
        .filter_map(|line| line.parse().ok())
        .collect();

    let numbers = parts
        .next()
        .unwrap_or("")
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| {
            line.split(',')
                .filter_map(|n| n.trim().parse().ok())
                .collect()
        })
        .collect();

    (constraints, numbers)
}
