use std::{
    collections::VecDeque,
    fmt::{Display, Write},
    thread::sleep,
    time::Duration,
};

use matrix::{Element, format::Conventional};

use crate::util::{Direction, Point};

#[derive(Clone, Copy, PartialEq)]
enum Position {
    Wall,
    Empty,
    Box,
    BoxLeft,
    BoxRight,
}

impl Element for Position {
    fn zero() -> Self {
        Position::Empty
    }
}

struct Game {
    matrix: Conventional<Position>,
    robot_position: Point,
    moves: VecDeque<Direction>,
}

impl Game {
    fn from_string(input: &str, part2: bool) -> Self {
        let mut parts = input.split("\n\n");

        let matrix_input = parts.next().unwrap();
        let rows = matrix_input.lines().count();
        let cols = matrix_input.lines().next().unwrap().chars().count();

        let mut matrix = if part2 {
            Conventional::new((rows, cols * 2))
        } else {
            Conventional::new((rows, cols))
        };

        let mut robot_position = (0, 0);

        for (row, line) in matrix_input.lines().enumerate() {
            for (col, c) in line.chars().enumerate() {
                if part2 {
                    match c {
                        '#' => {
                            matrix[(row, col * 2)] = Position::Wall;
                            matrix[(row, col * 2 + 1)] = Position::Wall;
                        }
                        '.' => {
                            matrix[(row, col * 2)] = Position::Empty;
                            matrix[(row, col * 2 + 1)] = Position::Empty;
                        }
                        'O' => {
                            matrix[(row, col * 2)] = Position::BoxLeft;
                            matrix[(row, col * 2 + 1)] = Position::BoxRight;
                        }
                        '@' => {
                            robot_position = (row, col * 2);
                            matrix[(row, col * 2)] = Position::Empty;
                            matrix[(row, col * 2 + 1)] = Position::Empty;
                        }
                        _ => panic!("Invalid character: {}", c),
                    };
                } else {
                    let position = match c {
                        '#' => Position::Wall,
                        '.' => Position::Empty,
                        'O' => Position::Box,
                        '@' => {
                            robot_position = (row, col);
                            Position::Empty
                        }
                        _ => panic!("Invalid character: {}", c),
                    };

                    matrix[(row, col)] = position;
                };
            }
        }

        let moves_input = parts.next().unwrap();
        let moves = moves_input
            .chars()
            .filter(|v| *v != '\n')
            .map(|c| match c {
                '^' => Direction::Up,
                'v' => Direction::Down,
                '<' => Direction::Left,
                '>' => Direction::Right,
                _ => panic!("Invalid move: {}", c),
            })
            .collect();

        Self {
            matrix,
            robot_position: robot_position.into(),
            moves,
        }
    }

    fn check_move(&self, position: Point, direction: Direction) -> bool {
        let mut stack = vec![position];

        while let Some(pos) = stack.pop() {
            let new_position = pos.to(direction);
            let block_at = self.matrix[new_position];

            match block_at {
                // We blocked
                Position::Wall => return false,
                // Open space, all good, nothing to do
                Position::Empty => {}
                // Check the next space
                Position::Box => {
                    stack.push(new_position);
                }
                Position::BoxLeft => {
                    stack.push(new_position);
                    if direction == Direction::Up || direction == Direction::Down {
                        stack.push(new_position.to(Direction::Right));
                    }
                }
                Position::BoxRight => {
                    stack.push(new_position);
                    if direction == Direction::Up || direction == Direction::Down {
                        stack.push(new_position.to(Direction::Left));
                    }
                }
            };
        }

        true
    }

    fn r#move(&mut self, position: Point, direction: Direction) {
        let mut stack = vec![position];

        while let Some(pos) = stack.pop() {
            let new_position = pos.to(direction);
            let block_at = self.matrix[new_position];

            match block_at {
                Position::Wall => panic!("Wall at {new_position:?}"),
                Position::Empty => {
                    self.matrix[new_position] = self.matrix[pos];
                    self.matrix[pos] = Position::Empty;
                }
                Position::Box => {
                    stack.push(pos);
                    stack.push(new_position);
                }
                Position::BoxLeft => {
                    stack.push(pos);
                    stack.push(new_position);
                    if direction == Direction::Up || direction == Direction::Down {
                        stack.push(new_position.to(Direction::Right));
                    }
                }
                Position::BoxRight => {
                    stack.push(pos);
                    stack.push(new_position);
                    if direction == Direction::Up || direction == Direction::Down {
                        stack.push(new_position.to(Direction::Left));
                    }
                }
            }
        }
    }

    fn step(&mut self) -> bool {
        if let Some(current_move) = self.moves.pop_front() {
            let can_move = self.check_move(self.robot_position, current_move);

            if can_move {
                self.r#move(self.robot_position, current_move);
                self.robot_position = self.robot_position.to(current_move);
            }

            true
        } else {
            false
        }
    }

    fn step_all(&mut self) {
        while self.step() {}
    }

    #[allow(dead_code)]
    fn step_print(&mut self) -> bool {
        let result = self.step();
        println!("{self}");
        result
    }

    #[allow(dead_code)]
    fn step_print_all(&mut self, delay: Option<u64>) {
        println!("{self}");

        if let Some(delay) = delay {
            sleep(Duration::from_millis(delay));
        }

        while self.step_print() {
            if let Some(delay) = delay {
                sleep(Duration::from_millis(delay));
            }
        }
    }

    fn gps_score_for(&self, (row, col): (usize, usize)) -> u64 {
        let target = self.matrix[(row, col)];
        if target == Position::Box || target == Position::BoxLeft {
            (100 * row + col) as u64
        } else {
            0
        }
    }

    fn game_score(&self) -> u64 {
        let rows = self.matrix.rows;
        let columns = self.matrix.columns;
        let mut score = 0;
        for row in 0..rows {
            for col in 0..columns {
                score += self.gps_score_for((row, col));
            }
        }
        score
    }
}

impl Display for Game {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let rows = self.matrix.rows;
        let columns = self.matrix.columns;

        for row in 0..rows {
            for col in 0..columns {
                let position = self.matrix[(row, col)];
                let c = if self.robot_position == (row, col).into() {
                    '@'
                } else {
                    match position {
                        Position::Wall => '#',
                        Position::Empty => '.',
                        Position::Box => 'O',
                        Position::BoxLeft => '[',
                        Position::BoxRight => ']',
                    }
                };
                f.write_char(c)?;
            }
            f.write_char('\n')?;
        }

        Ok(())
    }
}

pub fn part1(input: &str) -> u64 {
    let mut game = Game::from_string(input, false);

    game.step_all();

    game.game_score()
}

pub fn part2(input: &str) -> u64 {
    let mut game = Game::from_string(input, true);

    game.step_all();

    game.game_score()
}
