use std::collections::HashSet;

use indicatif::ProgressBar;
use matrix::prelude::*;

pub fn part1(input: &str) -> u64 {
    let mut state = State::read_from_string(input);

    while state.running {
        state.step();
    }

    state.visited.len() as u64
}

pub fn part2(input: &str) -> u64 {
    let state = State::read_from_string(input);

    let mut count = 0;

    let progress = ProgressBar::new(state.map.rows() as u64);

    for row in 0..state.map.rows() - 1 {
        for col in 0..state.map.columns() - 1 {
            let mut state = state.clone();
            state.map[&Location { row, col }] = LocState::Blocked;

            loop {
                state.step();

                if !state.running || state.saw_repeat {
                    break;
                }
            }

            if state.saw_repeat {
                count += 1;
            }
        }
        progress.inc(1);
    }

    count
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum LocState {
    Clear,
    Blocked,
}

impl std::fmt::Display for LocState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LocState::Clear => write!(f, "."),
            LocState::Blocked => write!(f, "#"),
        }
    }
}

impl Element for LocState {
    fn zero() -> Self {
        LocState::Clear
    }
}

impl From<char> for LocState {
    fn from(c: char) -> Self {
        match c {
            '.' => LocState::Clear,
            '>' | '<' | '^' | 'v' => LocState::Clear,
            '#' => LocState::Blocked,
            _ => panic!("Invalid input"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn turn_right(&self) -> Direction {
        match self {
            Direction::Up => Direction::Right,
            Direction::Down => Direction::Left,
            Direction::Left => Direction::Up,
            Direction::Right => Direction::Down,
        }
    }
}

impl From<char> for Direction {
    fn from(c: char) -> Self {
        match c {
            '>' => Direction::Right,
            '<' => Direction::Left,
            '^' => Direction::Up,
            'v' => Direction::Down,
            _ => panic!("Invalid input"),
        }
    }
}

impl std::fmt::Display for Direction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Direction::Up => write!(f, "^"),
            Direction::Down => write!(f, "v"),
            Direction::Left => write!(f, "<"),
            Direction::Right => write!(f, ">"),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
struct Location {
    row: usize,
    col: usize,
}

impl Position for &Location {
    fn row(&self) -> usize {
        self.row
    }

    fn column(&self) -> usize {
        self.col
    }
}

#[derive(Clone)]
struct State {
    location: Location,
    direction: Direction,
    map: Conventional<LocState>,
    visited: HashSet<Location>,
    visited_with_direction: HashSet<(Location, Direction)>,
    running: bool,
    saw_repeat: bool,
}

impl State {
    fn read_from_string(input: &str) -> State {
        let cols = input.lines().next().unwrap().len();
        let rows = input.lines().count();

        let values = input
            .chars()
            .filter(|c| *c != '\n')
            .map(LocState::from)
            .collect::<Vec<_>>();
        let matrix = Conventional::from_vec((rows, cols), values);

        let guard_index = input
            .chars()
            .filter(|c| *c != '\n')
            .position(|c| ['<', '>', '^', 'v'].contains(&c))
            .expect("Couldn't find guard :c");
        let guard_char = input
            .chars()
            .filter(|c| *c != '\n')
            .nth(guard_index)
            .expect("Couldn't find guard :c");

        let guard_row = guard_index / cols;
        let guard_col = guard_index % cols;

        let location = Location {
            row: guard_row,
            col: guard_col,
        };

        State {
            location,
            direction: guard_char.into(),
            map: matrix.transpose(),
            visited: HashSet::from([location]),
            visited_with_direction: HashSet::from([(location, guard_char.into())]),
            running: true,
            saw_repeat: false,
        }
    }

    fn facing_location(&self) -> Option<Location> {
        let rows = self.map.rows();
        let cols = self.map.columns();
        match self.direction {
            Direction::Up => {
                if self.location.row == 0 {
                    None
                } else {
                    Some(Location {
                        row: self.location.row - 1,
                        col: self.location.col,
                    })
                }
            }
            Direction::Down => {
                if self.location.row >= rows - 1 {
                    None
                } else {
                    Some(Location {
                        row: self.location.row + 1,
                        col: self.location.col,
                    })
                }
            }
            Direction::Left => {
                if self.location.col == 0 {
                    None
                } else {
                    Some(Location {
                        row: self.location.row,
                        col: self.location.col - 1,
                    })
                }
            }
            Direction::Right => {
                if self.location.col >= cols - 1 {
                    None
                } else {
                    Some(Location {
                        row: self.location.row,
                        col: self.location.col + 1,
                    })
                }
            }
        }
    }

    fn step(&mut self) {
        if let Some(location) = self.facing_location() {
            let facing = self.map[&location];

            match facing {
                LocState::Clear => self.location = location,
                LocState::Blocked => self.direction = self.direction.turn_right(),
            }

            if self
                .visited_with_direction
                .contains(&(self.location, self.direction))
            {
                self.saw_repeat = true;
            }

            self.visited.insert(self.location);
            self.visited_with_direction
                .insert((self.location, self.direction));
        } else {
            self.running = false;
        }
    }
}
