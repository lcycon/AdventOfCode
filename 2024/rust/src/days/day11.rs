use std::collections::HashMap;

fn parse_input(input: &str) -> Vec<u64> {
    input
        .trim()
        .split_ascii_whitespace()
        .map(|v| v.parse().unwrap())
        .collect()
}

fn count_blinks(stones: &[u64], days: usize) -> usize {
    let mut cache = HashMap::new();
    count_blinks_inner(stones, days, &mut cache)
}

fn count_blinks_inner(
    stones: &[u64],
    days: usize,
    cache: &mut HashMap<(u64, usize), usize>,
) -> usize {
    if days == 0 {
        stones.len()
    } else {
        stones
            .iter()
            .map(|v| match *v {
                v if cache.contains_key(&(v, days)) => cache[&(v, days)],
                0 => count_blinks_inner(&[1], days - 1, cache),
                v if v.to_string().len() % 2 == 0 => {
                    let string_val = v.to_string();
                    let (fst, snd) = string_val.split_at(string_val.len() / 2);

                    let result = count_blinks_inner(
                        &[fst.parse().unwrap(), snd.parse().unwrap()],
                        days - 1,
                        cache,
                    );

                    cache.insert((v, days), result);

                    result
                }
                v => {
                    let result = count_blinks_inner(&[v * 2024], days - 1, cache);

                    cache.insert((v, days), result);

                    result
                }
            })
            .sum()
    }
}

pub fn part1(input: &str) -> u64 {
    let stones = parse_input(input);
    let count = count_blinks(&stones, 25);

    count as u64
}

pub fn part2(input: &str) -> u64 {
    let stones = parse_input(input);
    let count = count_blinks(&stones, 75);

    count as u64
}
