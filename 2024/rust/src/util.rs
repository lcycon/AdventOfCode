use std::ops::Add;

use matrix::Position;
use strum::EnumIter;

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug)]
pub struct Point {
    pub row: isize,
    pub col: isize,
}

impl From<(usize, usize)> for Point {
    fn from(value: (usize, usize)) -> Self {
        Point {
            row: value.0 as isize,
            col: value.1 as isize,
        }
    }
}

// This just explodes, it's easier for this use-case
impl Position for Point {
    fn row(&self) -> usize {
        self.row.try_into().unwrap()
    }

    fn column(&self) -> usize {
        self.col.try_into().unwrap()
    }
}

impl Add for Point {
    type Output = Point;

    fn add(self, other: Point) -> Point {
        Point {
            row: self.row + other.row,
            col: self.col + other.col,
        }
    }
}

impl Point {
    pub fn to(&self, direction: Direction) -> Point {
        *self + direction.to_offset()
    }

    pub fn bounded(&self, rows: usize, cols: usize) -> Option<Point> {
        if self.row < 0 || self.col < 0 || self.row >= rows as isize || self.col >= cols as isize {
            None
        } else {
            Some(*self)
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, Debug, EnumIter)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
    UpRight,
    UpLeft,
    DownRight,
    DownLeft,
}

impl Direction {
    pub const ORTHOGONAL: [Direction; 4] = [
        Direction::Up,
        Direction::Down,
        Direction::Left,
        Direction::Right,
    ];

    pub fn to_offset(&self) -> Point {
        match self {
            Direction::Up => Point { row: -1, col: 0 },
            Direction::Down => Point { row: 1, col: 0 },
            Direction::Left => Point { row: 0, col: -1 },
            Direction::Right => Point { row: 0, col: 1 },
            Direction::UpRight => Point { row: -1, col: 1 },
            Direction::UpLeft => Point { row: -1, col: -1 },
            Direction::DownRight => Point { row: 1, col: 1 },
            Direction::DownLeft => Point { row: 1, col: -1 },
        }
    }
}
