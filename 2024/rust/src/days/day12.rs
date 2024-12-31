use std::collections::HashSet;

use lazy_static::lazy_static;
use matrix::{Element, format::Conventional, prelude::Transpose};
use strum::IntoEnumIterator;

use crate::util::Direction;

lazy_static! {
    static ref EXTERIOR_CORNERS: [HashSet<crate::util::Direction>; 4] = [
        [Direction::Up, Direction::Left].into(),
        [Direction::Up, Direction::Right].into(),
        [Direction::Down, Direction::Left].into(),
        [Direction::Down, Direction::Right].into(),
    ];
}

#[derive(PartialEq, Clone, Copy, Debug)]
struct Plant(char);

impl Element for Plant {
    fn zero() -> Self {
        Plant('0')
    }
}

impl From<char> for Plant {
    fn from(value: char) -> Self {
        Self(value)
    }
}

struct Board {
    matrix: Conventional<Plant>,
}

impl Board {
    fn from_string(input: &str) -> Self {
        let cols = input.lines().next().unwrap().len();
        let rows = input.lines().count();

        let values: Vec<Plant> = input
            .chars()
            .filter(|c| *c != '\n')
            .map(|v| v.into())
            .collect::<Vec<_>>();
        let matrix = Conventional::from_vec((rows, cols), values);

        Board {
            matrix: matrix.transpose(),
        }
    }

    fn regions(&self) -> Vec<Region> {
        let mut regions = vec![];
        let mut visited = HashSet::new();

        for row in 0..self.matrix.rows {
            for col in 0..self.matrix.columns {
                if visited.contains(&(row, col)) {
                    continue;
                }

                let region = self.region_from_start(&mut visited, (row, col));
                regions.push(region);
            }
        }

        regions
    }

    fn region_from_start(
        &self,
        visited: &mut HashSet<(usize, usize)>,
        start: (usize, usize),
    ) -> Region {
        let mut to_visit = vec![start];
        let start_plant = self.matrix[start];

        let mut size = 0;
        let mut corners = 0;
        let mut total_border = 0;

        while let Some(current) = to_visit.pop() {
            if visited.contains(&current) {
                continue;
            }

            if self.matrix[current] != start_plant {
                continue;
            }

            let mut borders = 4;
            let neighbors = self.same_neighbors(current, start_plant);

            if neighbors.contains(&Direction::Up) {
                borders -= 1;
                to_visit.push(
                    Direction::Up
                        .calculate_new_coords(current, self.matrix.rows, self.matrix.columns)
                        .unwrap(),
                );
            }

            if neighbors.contains(&Direction::Down) {
                borders -= 1;
                to_visit.push(
                    Direction::Down
                        .calculate_new_coords(current, self.matrix.rows, self.matrix.columns)
                        .unwrap(),
                );
            }

            if neighbors.contains(&Direction::Left) {
                borders -= 1;
                to_visit.push(
                    Direction::Left
                        .calculate_new_coords(current, self.matrix.rows, self.matrix.columns)
                        .unwrap(),
                );
            }

            if neighbors.contains(&Direction::Right) {
                borders -= 1;
                to_visit.push(
                    Direction::Right
                        .calculate_new_coords(current, self.matrix.rows, self.matrix.columns)
                        .unwrap(),
                );
            }

            let exterior_corners = EXTERIOR_CORNERS
                .iter()
                .filter(|v| neighbors.is_disjoint(v))
                .count();
            corners += exterior_corners;

            if neighbors.is_superset(&[Direction::Up, Direction::Left].into())
                && !neighbors.contains(&Direction::UpLeft)
            {
                corners += 1;
            }

            if neighbors.is_superset(&[Direction::Up, Direction::Right].into())
                && !neighbors.contains(&Direction::UpRight)
            {
                corners += 1;
            }

            if neighbors.is_superset(&[Direction::Down, Direction::Left].into())
                && !neighbors.contains(&Direction::DownLeft)
            {
                corners += 1;
            }

            if neighbors.is_superset(&[Direction::Down, Direction::Right].into())
                && !neighbors.contains(&Direction::DownRight)
            {
                corners += 1;
            }

            size += 1;
            total_border += borders;
            visited.insert(current);
        }

        Region {
            _plant: start_plant,
            size,
            border_size: total_border,
            corners,
        }
    }

    fn same_neighbors(&self, (row, col): (usize, usize), plant: Plant) -> HashSet<Direction> {
        crate::util::Direction::iter()
            .filter(|direction| {
                let new_coord = direction.calculate_new_coords(
                    (row, col),
                    self.matrix.rows,
                    self.matrix.columns,
                );

                if let Some(new_coord) = new_coord {
                    self.matrix[new_coord] == plant
                } else {
                    false
                }
            })
            .collect()
    }
}

#[derive(Debug)]
struct Region {
    _plant: Plant,
    size: usize,
    border_size: usize,
    corners: usize,
}

impl Region {
    fn price(&self) -> u64 {
        (self.size as u64) * (self.border_size as u64)
    }

    fn price2(&self) -> u64 {
        (self.size as u64) * (self.corners as u64)
    }
}

pub fn part1(input: &str) -> u64 {
    let state = Board::from_string(input);
    let regions = state.regions();

    let total_price: u64 = regions.iter().map(|region| region.price()).sum();

    total_price
}

pub fn part2(input: &str) -> u64 {
    let state = Board::from_string(input);
    let regions = state.regions();

    let total_price: u64 = regions.iter().map(|region| region.price2()).sum();

    total_price
}
