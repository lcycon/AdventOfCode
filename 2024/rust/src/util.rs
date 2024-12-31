use strum::EnumIter;

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
    pub fn calculate_new_coords(
        &self,
        (row, col): (usize, usize),
        rows: usize,
        columns: usize,
    ) -> Option<(usize, usize)> {
        let (new_row, new_col) = match self {
            Direction::Up => (row.checked_sub(1)?, col),
            Direction::Down => (row + 1, col),
            Direction::Left => (row, col.checked_sub(1)?),
            Direction::Right => (row, col + 1),
            Direction::UpRight => (row.checked_sub(1)?, col + 1),
            Direction::UpLeft => (row.checked_sub(1)?, col.checked_sub(1)?),
            Direction::DownRight => (row + 1, col + 1),
            Direction::DownLeft => (row + 1, col.checked_sub(1)?),
        };

        if new_row < rows && new_col < columns {
            Some((new_row, new_col))
        } else {
            None
        }
    }
}
