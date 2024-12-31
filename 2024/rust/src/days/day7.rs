use easy_smt::{Context, ContextBuilder, Response, SExpr};
use indicatif::ProgressBar;
use itertools::Itertools;

fn ifchain(ctx: &Context, a: SExpr, conds: &[(SExpr, SExpr)]) -> SExpr {
    let mut result = ctx.true_();

    for (cond, body) in conds {
        result = ctx.ite(ctx.eq(a, *cond), *body, result);
    }

    result
}

fn solve_single_part1(total: u64, numbers: &[u64]) -> std::io::Result<bool> {
    let operations_length = numbers.len() - 1;
    let results_length = numbers.len();

    let mut ctx = ContextBuilder::new()
        .solver("z3", ["-smt2", "-in"])
        .build()?;

    let mut operations = Vec::new();

    for i in 0..operations_length {
        let op = ctx.declare_const(format!("op_{}", i), ctx.int_sort())?;
        ctx.assert(ctx.or(ctx.eq(op, ctx.numeral(0)), ctx.eq(op, ctx.numeral(1))))?;
        operations.push(op);
    }

    let mut results = Vec::new();

    for i in 0..results_length {
        let op = ctx.declare_const(format!("result_{}", i), ctx.int_sort())?;
        results.push(op);
    }
    ctx.assert(ctx.eq(results[0], ctx.numeral(numbers[0])))?;

    for i in 0..operations_length {
        ctx.assert(ifchain(&ctx, operations[i], &[
            (
                ctx.numeral(0),
                ctx.eq(
                    results[i + 1],
                    ctx.plus(results[i], ctx.numeral(numbers[i + 1])),
                ),
            ),
            (
                ctx.numeral(1),
                ctx.eq(
                    results[i + 1],
                    ctx.times(results[i], ctx.numeral(numbers[i + 1])),
                ),
            ),
        ]))?;
    }

    ctx.assert(ctx.eq(*results.last().unwrap(), ctx.numeral(total)))?;

    let result = ctx.check()?;

    Ok(result == Response::Sat)
}

fn parse_line(input: &str) -> (u64, Vec<u64>) {
    let (target, numbers) = input.split_once(":").unwrap();
    let target = target.parse().unwrap();
    let numbers = numbers
        .split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect();
    (target, numbers)
}

pub fn part1(input: &str) -> u64 {
    env_logger::init();

    let inputs = input.lines().map(parse_line).collect_vec();

    let mut total = 0;

    let progress = ProgressBar::new(inputs.len() as u64);

    for (target, numbers) in inputs {
        if solve_single_part1(target, &numbers).unwrap() {
            total += target;
        }
        progress.inc(1);
    }

    total
}

pub fn part2(input: &str) -> u64 {
    env_logger::init();

    let inputs = input.lines().map(parse_line).collect_vec();

    let mut total = 0;

    let progress = ProgressBar::new(inputs.len() as u64);

    for (target, numbers) in inputs {
        if solve_single_part2(target, &numbers).unwrap() {
            total += target;
        }
        progress.inc(1);
    }

    total
}

fn z3_power(ctx: &Context, base: SExpr, exponent: SExpr) -> SExpr {
    ctx.list(vec![ctx.atom("^"), base, exponent])
}

fn generate_concatenation(
    ctx: &mut Context,
    prefix: usize,
    expr1: SExpr,
    expr2: SExpr,
) -> std::io::Result<SExpr> {
    // Calculate 10^n for the length of expr2
    let display1 = ctx.display(expr1).to_string();
    let display2 = ctx.display(expr2).to_string();
    let mut digits = ctx.numeral(1);

    for i in 1..11 {
        digits = ctx.ite(
            ctx.gte(expr2, z3_power(ctx, ctx.numeral(10), ctx.numeral(i))),
            ctx.numeral(i + 1),
            digits,
        );
    }

    let tens_digits = ctx.declare_const(
        format!("tens_digits_{}_{}", prefix, display2),
        ctx.int_sort(),
    )?;
    ctx.assert(ctx.eq(tens_digits, z3_power(ctx, ctx.numeral(10), digits)))?;

    let concat = ctx.declare_const(
        format!("concat_{}_{}_{}", prefix, display1, display2),
        ctx.int_sort(),
    )?;
    ctx.assert(ctx.eq(concat, ctx.plus(expr2, ctx.times(expr1, tens_digits))))?;

    Ok(concat)
}

fn solve_single_part2(total: u64, numbers: &[u64]) -> std::io::Result<bool> {
    let operations_length = numbers.len() - 1;
    let results_length = numbers.len();

    let mut ctx = ContextBuilder::new()
        .solver("z3", ["-smt2", "-in"])
        .build()?;

    let mut operations = Vec::new();

    for i in 0..operations_length {
        let op = ctx.declare_const(format!("op_{}", i), ctx.int_sort())?;
        ctx.assert(ctx.or_many(vec![
            ctx.eq(op, ctx.numeral(0)),
            ctx.eq(op, ctx.numeral(1)),
            ctx.eq(op, ctx.numeral(2)),
        ]))?;
        operations.push(op);
    }

    let mut results = Vec::new();

    for i in 0..results_length {
        let op = ctx.declare_const(format!("result_{}", i), ctx.int_sort())?;
        results.push(op);
    }
    ctx.assert(ctx.eq(results[0], ctx.numeral(numbers[0])))?;

    for i in 0..operations_length {
        let next_number = ctx.numeral(numbers[i + 1]);
        let concat = generate_concatenation(&mut ctx, i, results[i], next_number)?;
        ctx.assert(ifchain(&ctx, operations[i], &[
            (
                ctx.numeral(0),
                ctx.eq(
                    results[i + 1],
                    ctx.plus(results[i], ctx.numeral(numbers[i + 1])),
                ),
            ),
            (
                ctx.numeral(1),
                ctx.eq(
                    results[i + 1],
                    ctx.times(results[i], ctx.numeral(numbers[i + 1])),
                ),
            ),
            (ctx.numeral(2), ctx.eq(results[i + 1], concat)),
        ]))?;
    }

    ctx.assert(ctx.eq(*results.last().unwrap(), ctx.numeral(total)))?;

    let result = ctx.check()?;

    Ok(result == Response::Sat)
}
