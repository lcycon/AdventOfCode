use itertools::Itertools;

#[derive(Debug, Clone)]
struct Program {
    pub register_a: u64,
    register_b: u64,
    register_c: u64,
    ip: usize,
    instructions: Vec<u8>,
    output: Vec<u8>,
}

impl Program {
    fn from_string(input: &str) -> Self {
        let mut lines = input.lines();

        let register_a = lines
            .next()
            .and_then(|line| line.strip_prefix("Register A: "))
            .and_then(|value| value.parse::<u64>().ok())
            .unwrap_or(0);

        let register_b = lines
            .next()
            .and_then(|line| line.strip_prefix("Register B: "))
            .and_then(|value| value.parse::<u64>().ok())
            .unwrap_or(0);

        let register_c = lines
            .next()
            .and_then(|line| line.strip_prefix("Register C: "))
            .and_then(|value| value.parse::<u64>().ok())
            .unwrap_or(0);

        lines.next();

        let instructions = lines
            .next()
            .and_then(|line| line.strip_prefix("Program: "))
            .map(|program| {
                program
                    .split(',')
                    .filter_map(|num| num.parse::<u8>().ok())
                    .collect::<Vec<u8>>()
            })
            .unwrap_or_default();

        Self {
            register_a,
            register_b,
            register_c,
            ip: 0,
            instructions,
            output: vec![],
        }
    }

    fn combo_operand(&mut self, idx: usize) -> u64 {
        match self.instructions[idx] {
            v @ (0..=3) => v as u64,
            4 => self.register_a,
            5 => self.register_b,
            6 => self.register_c,
            _ => panic!("Shouldn't ever happen"),
        }
    }

    fn execute_cycle(&mut self) {
        let instruction = self.instructions[self.ip];
        let mut did_jump = false;

        match instruction {
            0 => {
                let result = self.register_a
                    / 2_u64.pow(self.combo_operand(self.ip + 1).try_into().unwrap());
                self.register_a = result;
            }
            1 => {
                let result = self.register_b ^ self.instructions[self.ip + 1] as u64;
                self.register_b = result;
            }
            2 => {
                let result = self.combo_operand(self.ip + 1) % 8;
                self.register_b = result;
            }
            3 => {
                if self.register_a != 0 {
                    self.ip = self.instructions[self.ip + 1] as usize;
                    did_jump = true;
                }
            }
            4 => {
                let result = self.register_b ^ self.register_c;
                self.register_b = result;
            }
            5 => {
                let result = self.combo_operand(self.ip + 1) % 8;
                self.output.push(result.try_into().unwrap());
            }
            6 => {
                let result = self.register_a
                    / 2_u64.pow(self.combo_operand(self.ip + 1).try_into().unwrap());
                self.register_b = result;
            }
            7 => {
                let result = self.register_a
                    / 2_u64.pow(self.combo_operand(self.ip + 1).try_into().unwrap());
                self.register_c = result;
            }
            _ => panic!("Unsupported opcode"),
        }

        if !did_jump {
            self.ip += 2;
        }
    }

    fn run(&mut self) {
        while self.ip < self.instructions.len() {
            self.execute_cycle();
        }
    }

    fn run_to_output(&mut self) -> Option<u8> {
        let next_out_length = self.output.len() + 1;

        while self.output.len() != next_out_length && self.ip < self.instructions.len() {
            self.execute_cycle();
        }

        if self.ip >= self.instructions.len() {
            None
        } else {
            Some(*self.output.last().unwrap())
        }
    }
}

pub fn part1(input: &str) -> u64 {
    let mut program = Program::from_string(input);

    println!("Program: {program:?}");

    program.run();

    println!("Program: {program:?}");

    let result = program
        .output
        .iter()
        .map(|v| v.to_string())
        .collect_vec()
        .join(",");

    println!("Result: {result}");

    0
}

pub fn part2(input: &str) -> u64 {
    let program = Program::from_string(input);

    println!("Program: {:?}", program);

    let answer = recurse_part2(&program, program.instructions.len() - 1, 0).unwrap();

    let mut tmp = program.clone();
    tmp.register_a = answer;
    tmp.run();

    assert_eq!(tmp.instructions, tmp.output);

    answer
}

fn recurse_part2(program: &Program, idx: usize, input: u64) -> Option<u64> {
    for lower in 0..8 {
        let new_num = (input << 3) | lower;

        let mut tmp = program.clone();
        tmp.register_a = new_num;
        let output = tmp.run_to_output().unwrap();

        if output == program.instructions[idx] {
            if idx == 0 {
                return Some(new_num);
            } else {
                let result = recurse_part2(program, idx - 1, new_num);

                if let Some(result) = result {
                    return Some(result);
                }
            }
        }
    }

    None
}
