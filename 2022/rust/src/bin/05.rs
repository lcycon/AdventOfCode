#[derive(Debug, PartialEq, Eq)]
pub enum CrateInput {
    Blank,
    Crate(char),
}

impl CrateInput {
    pub fn as_crate(&self) -> Option<Crate> {
        match self {
            CrateInput::Blank => None,
            CrateInput::Crate(ch) => Some(Crate(*ch)),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Rank(pub Vec<CrateInput>);

#[derive(Debug, PartialEq, Eq)]
pub struct Crate(char);

#[derive(Debug, PartialEq, Eq)]
pub struct GameState(Vec<Vec<Crate>>);

impl GameState {
    pub fn apply_move(&mut self, mv: &Move) {
        for _ in 0..mv.count {
            let elem = self.0.get_mut(mv.from).unwrap().pop().unwrap();
            self.0.get_mut(mv.to).unwrap().push(elem);
        }
    }

    pub fn apply_move_p2(&mut self, mv: &Move) {
        let mut elems = {
            let from = self.0.get_mut(mv.from).unwrap();
            let len = from.len();
            from.split_off(len - mv.count)
        };
        self.0.get_mut(mv.to).unwrap().append(&mut elems);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Move {
    pub count: usize,
    pub from: usize,
    pub to: usize,
}

pub fn part_one(input: &str) -> Option<String> {
    let (input, mut game_state) = parser::game_state(input).unwrap();
    let (_, moves) = parser::moves(input).unwrap();

    for mv in moves {
        game_state.apply_move(&mv);
    }

    let result: String = game_state.0.iter().map(|r| r.last().unwrap().0).collect();

    Some(result)
}

pub fn part_two(input: &str) -> Option<String> {
    let (input, mut game_state) = parser::game_state(input).unwrap();
    let (_, moves) = parser::moves(input).unwrap();

    for mv in moves {
        game_state.apply_move_p2(&mv);
    }

    let result: String = game_state.0.iter().map(|r| r.last().unwrap().0).collect();

    Some(result)
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 5);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

mod parser {
    use nom::branch::alt;
    use nom::bytes::complete::*;
    use nom::character::complete::*;
    use nom::character::is_alphabetic;
    use nom::character::is_newline;
    use nom::combinator::*;
    use nom::multi::separated_list1;
    use nom::sequence::tuple;
    use nom::IResult;

    use crate::{Crate, CrateInput, GameState, Move, Rank};

    fn empty_crate(input: &str) -> IResult<&str, CrateInput> {
        map(tag("   "), |_| CrateInput::Blank)(input)
    }

    fn filled_crate(input: &str) -> IResult<&str, CrateInput> {
        let (input, _) = char('[')(input)?;
        let (input, crate_char) = satisfy(|v| is_alphabetic(v as u8))(input)?;
        let (input, _) = char(']')(input)?;

        Ok((input, CrateInput::Crate(crate_char)))
    }

    fn crate_input(input: &str) -> IResult<&str, CrateInput> {
        alt((filled_crate, empty_crate))(input)
    }

    fn rank(input: &str) -> IResult<&str, Rank> {
        map(separated_list1(char(' '), crate_input), Rank)(input)
    }

    pub fn game_state(input: &str) -> IResult<&str, GameState> {
        let (input, mut ranks) = separated_list1(line_ending, rank)(input)?;
        let (input, _) = tuple((
            newline,
            take_till(|c| is_newline(c as u8)),
            newline,
            newline,
        ))(input)?;

        ranks.reverse();

        let length = ranks.first().unwrap().0.len();
        let mut stacks: Vec<Vec<Crate>> = Vec::with_capacity(length);

        for idx in 0..length {
            let vec = ranks
                .iter()
                .filter_map(|r| r.0.get(idx).and_then(|ci| ci.as_crate()))
                .collect();

            stacks.push(vec);
        }

        Ok((input, GameState(stacks)))
    }

    fn p_move(input: &str) -> IResult<&str, Move> {
        let (input, _) = tag("move ")(input)?;
        let (input, count) = digit1(input)?;
        let (input, _) = tag(" from ")(input)?;
        let (input, from) = digit1(input)?;
        let (input, _) = tag(" to ")(input)?;
        let (input, to) = digit1(input)?;

        Ok((
            input,
            Move {
                count: count.parse().unwrap(),
                from: from.parse::<usize>().unwrap() - 1,
                to: to.parse::<usize>().unwrap() - 1,
            },
        ))
    }

    pub fn moves(input: &str) -> IResult<&str, Vec<Move>> {
        separated_list1(newline, p_move)(input)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 5);
        assert_eq!(part_one(&input), Some("CMZ".to_string()));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 5);
        assert_eq!(part_two(&input), Some("MCD".to_string()));
    }
}
