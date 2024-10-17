use std::{collections::HashMap, str::FromStr};

use itertools::Itertools;
use nom::InputTake;

struct File(String, u32);

#[derive(Default)]
struct Dir {
    dirs: HashMap<String, Dir>,
    files: Vec<File>,
}

impl Dir {
    pub fn add_file<T: AsRef<str>>(&mut self, dir: &[T], file: File) {
        match dir {
            [] => self.files.push(file),
            [pth, rest @ ..] => self
                .dirs
                .entry(pth.as_ref().to_string())
                .or_default()
                .add_file(rest, file),
        }
    }

    pub fn total_file_size(&self) -> u32 {
        let sub_sizes: u32 = self
            .dirs
            .values()
            .into_iter()
            .map(|v| v.total_file_size())
            .sum();
        let file_sizes: u32 = self.files.iter().map(|v| v.1).sum();

        sub_sizes + file_sizes
    }
}

impl<'a> IntoIterator for &'a Dir {
    type Item = &'a Dir;
    type IntoIter = std::vec::IntoIter<&'a Dir>;

    fn into_iter(self) -> Self::IntoIter {
        fn go<'a>(dir: &'a Dir, v: &mut Vec<&'a Dir>) {
            v.push(dir);

            for dir in dir.dirs.values() {
                go(dir, v);
            }
        }

        let mut results = vec![];
        go(self, &mut results);
        results.into_iter()
    }
}

enum Command {
    Cd(String),
    Ls,
}

impl FromStr for Command {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.take(2) {
            "ls" => Ok(Command::Ls),
            "cd" => Ok(Command::Cd(s[3..].to_string())),
            _ => panic!("Unknown command"),
        }
    }
}

enum LineInput {
    Command(Command),
    Dir(String),
    File(u32, String),
}

impl FromStr for LineInput {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(command_string) = s.strip_prefix("$ ") {
            Ok(LineInput::Command(command_string.parse::<Command>()?))
        } else if let Some(dir) = s.strip_prefix("dir ") {
            Ok(LineInput::Dir(dir.to_string()))
        } else {
            let (size, name) = s.split_once(' ').unwrap();

            Ok(LineInput::File(size.parse().unwrap(), name.to_string()))
        }
    }
}

fn change_dir(dir: &mut Vec<String>, input: &str) {
    match input {
        "/" => {
            dir.drain(..);
        }
        ".." => {
            dir.pop();
        }
        ndir => dir.push(ndir.to_string()),
    }
}

fn process_input(inputs: &[LineInput]) -> Dir {
    let mut root = Dir::default();
    let mut path: Vec<String> = vec![];
    let mut adding_files = false;

    for input in inputs {
        match input {
            LineInput::Command(Command::Ls) => adding_files = true,
            LineInput::Command(Command::Cd(to_dir)) => {
                adding_files = false;
                change_dir(&mut path, to_dir)
            }
            LineInput::Dir(_) => (),
            LineInput::File(size, name) if adding_files => {
                root.add_file(&path, File(name.clone(), *size))
            }
            _ => (),
        }
    }

    root
}

pub fn part_one(input: &str) -> Option<u32> {
    let inputs = input
        .lines()
        .map(|v| v.parse::<LineInput>().unwrap())
        .collect_vec();

    let root = process_input(&inputs);

    let sum = root
        .into_iter()
        .map(|v| v.total_file_size())
        .filter(|v| *v <= 100000)
        .sum();

    Some(sum)
}

pub fn part_two(input: &str) -> Option<u32> {
    let inputs = input
        .lines()
        .map(|v| v.parse::<LineInput>().unwrap())
        .collect_vec();

    let root = process_input(&inputs);
    let total_size = root.total_file_size();
    let free_size = 70000000 - total_size;
    let needed_size = 30000000 - free_size;

    let chosen = root
        .into_iter()
        .map(|v| v.total_file_size())
        .filter(|size| *size >= needed_size)
        .sorted()
        .next()
        .unwrap();

    Some(chosen)
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 7);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 7);
        assert_eq!(part_one(&input), Some(95437));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 7);
        assert_eq!(part_two(&input), Some(24933642));
    }
}
