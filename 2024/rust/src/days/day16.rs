use std::{
    collections::{HashMap, HashSet},
    fmt::{Display, Write},
    u64,
};

use itertools::Itertools;
use matrix::{Element, format::Conventional};

use crate::util::{Direction, Point};

#[derive(PartialEq, Clone, Copy)]
enum Location {
    Wall,
    Free,
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Location::Wall => f.write_char('#'),
            Location::Free => f.write_char('.'),
        }
    }
}

impl Element for Location {
    fn zero() -> Self {
        Location::Wall
    }
}

struct Input {
    matrix: Conventional<Location>,
    start_position: Point,
    end_position: Point,
}

impl Input {
    fn from_string(input: &str) -> Input {
        let rows = input.lines().count();
        let cols = input.lines().next().unwrap().chars().count();

        let mut matrix = Conventional::new((rows, cols));

        let mut start_position = Point { row: 0, col: 0 };
        let mut end_position = Point { row: 0, col: 0 };

        for (row, line) in input.lines().enumerate() {
            for (col, c) in line.chars().enumerate() {
                let position = match c {
                    '#' => Location::Wall,
                    '.' => Location::Free,
                    'S' => {
                        start_position = (row, col).into();
                        Location::Free
                    }
                    'E' => {
                        end_position = (row, col).into();
                        Location::Free
                    }
                    _ => panic!("Invalid character: {}", c),
                };

                matrix[(row, col)] = position;
            }
        }

        Input {
            matrix,
            start_position,
            end_position,
        }
    }

    fn moves_from(&self, (point, direction): (Point, Direction)) -> Vec<((Point, Direction), u64)> {
        let mut moves = vec![];

        let front = point.to(direction);
        if self.matrix[front] == Location::Free {
            moves.push(((front, direction), 1));
        }

        Direction::ORTHOGONAL
            .into_iter()
            .filter(|v| *v != direction)
            .filter(|v| *v != direction.opposite())
            .for_each(|v| {
                let valid_turn = self.matrix[point.to(v)] == Location::Free;

                if valid_turn {
                    moves.push(((point, v), 1000));
                }
            });

        moves
    }

    fn smallest_maze_solve(&self) -> (u64, u64) {
        let mut dist: HashMap<(Point, Direction), u64> = HashMap::new();
        let mut queue = Vec::new();

        let mut prev = HashMap::new();

        dist.insert((self.start_position, Direction::Right), 0);
        queue.push((self.start_position, Direction::Right));

        while let Some(node) = queue.pop() {
            let neighbors = self.moves_from(node);

            for neighbor in neighbors {
                let new_cost = dist[&node] + neighbor.1;
                let prev_cost = *dist.get(&neighbor.0).unwrap_or(&u64::MAX);

                #[allow(clippy::comparison_chain)]
                if new_cost < prev_cost {
                    dist.entry(neighbor.0)
                        .and_modify(|v| *v = new_cost.min(*v))
                        .or_insert(new_cost);
                    queue.push(neighbor.0);
                    prev.insert(neighbor.0, vec![node]);
                } else if new_cost == prev_cost {
                    prev.get_mut(&neighbor.0).unwrap().push(node);
                }
            }
        }

        let cost = *Direction::ORTHOGONAL
            .into_iter()
            .map(|v| dist.get(&(self.end_position, v)).unwrap_or(&u64::MAX))
            .min()
            .unwrap();

        let mut visited = HashSet::new();
        let mut rbfs_queue = Direction::ORTHOGONAL
            .iter()
            .map(|d| (self.end_position, *d))
            .filter(|v| dist.get(v).unwrap_or(&u64::MAX) == &cost)
            .collect_vec();

        while let Some(node) = rbfs_queue.pop() {
            visited.insert(node.0);

            prev.get(&node)
                .unwrap_or(&vec![])
                .iter()
                .for_each(|v| rbfs_queue.push(*v));
        }

        self.print_full(&visited);

        (cost, visited.len() as u64)
    }

    fn print_full(&self, visited: &HashSet<Point>) {
        for row in 0..self.matrix.rows {
            for col in 0..self.matrix.columns {
                if visited.contains(&(row, col).into()) {
                    print!("O");
                } else {
                    print!("{}", self.matrix[(row, col)]);
                }
            }
            println!();
        }
    }
}

pub fn part1(input: &str) -> u64 {
    let game = Input::from_string(input);

    game.smallest_maze_solve().0
}

pub fn part2(input: &str) -> u64 {
    let game = Input::from_string(input);

    game.smallest_maze_solve().1
}
