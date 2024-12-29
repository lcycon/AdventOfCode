use std::fmt::Display;

use indicatif::ProgressBar;
use itertools::Itertools;

#[derive(PartialEq, Eq)]
enum Block {
    Empty,
    Full(u16),
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Empty => write!(f, "."),
            Self::Full(id) => write!(f, "{}", id),
        }
    }
}

struct Disk {
    blocks: Vec<Block>,
    num_files: u16,
}

impl Display for Disk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for block in &self.blocks {
            write!(f, "{}", block)?;
        }

        writeln!(f)
    }
}

impl Disk {
    fn from_string(input: &str) -> Self {
        let mut blocks = Vec::new();

        let segments = input.chars().map(|c| c.to_digit(10).unwrap()).collect_vec();
        let total_blocks: usize = segments.iter().sum::<u32>().try_into().unwrap();
        blocks.reserve(total_blocks);

        let mut is_file = true;
        let mut file_id = 0;

        for segment in segments {
            for _ in 0..segment {
                if is_file {
                    blocks.push(Block::Full(file_id));
                } else {
                    blocks.push(Block::Empty);
                }
            }

            is_file = !is_file;
            if is_file {
                file_id += 1;
            }
        }

        Disk {
            blocks,
            num_files: file_id + 1,
        }
    }

    fn reorder(&mut self) {
        let mut head_idx = 0;
        let mut tail_idx = self.blocks.len() - 1;

        while head_idx < tail_idx {
            if let Block::Empty = self.blocks[tail_idx] {
                tail_idx -= 1;
            } else if let Block::Empty = self.blocks[head_idx] {
                self.blocks.swap(head_idx, tail_idx);
                head_idx += 1;
                tail_idx -= 1;
            } else {
                head_idx += 1;
            }
        }
    }

    fn checksum(&self) -> u64 {
        self.blocks
            .iter()
            .enumerate()
            .filter_map(|(idx, block)| match block {
                Block::Empty => None,
                Block::Full(fid) => Some(idx as u64 * *fid as u64),
            })
            .sum()
    }

    fn reorder_whole_files(&mut self, show_progress: bool) {
        let inc: Box<dyn Fn()> = if show_progress {
            let progress = ProgressBar::new(self.num_files as u64);
            Box::new(move || {
                progress.inc(1);
            })
        } else {
            Box::new(|| {})
        };

        for file_num in (0..self.num_files).rev() {
            let file_start = self
                .blocks
                .iter()
                .position(|b| match b {
                    Block::Full(fid) => *fid == file_num,
                    Block::Empty => false,
                })
                .unwrap();

            let file_end = self
                .blocks
                .iter()
                .enumerate()
                .rev()
                .find(|b| match b.1 {
                    Block::Full(fid) => *fid == file_num,
                    Block::Empty => false,
                })
                .unwrap()
                .0;

            let length = file_end - file_start + 1;

            if let Some((start, end)) = self.find_availble_space_for(length) {
                if end < file_start {
                    let (fst, snd) = self.blocks.split_at_mut(file_start);
                    slice_swap(&mut fst[start..=end], &mut snd[0..length]);
                }
            }
            inc();
        }
    }

    fn find_availble_space_for(&self, size: usize) -> Option<(usize, usize)> {
        let chunked = self
            .blocks
            .iter()
            .enumerate()
            .chunk_by(|e| *e.1 == Block::Empty);

        let mut blocks = chunked
            .into_iter()
            .filter(|v| v.0)
            .map(|v| {
                let seq = v.1.collect_vec();
                let start = seq.first().unwrap().0;
                let end = seq.last().unwrap().0;
                let length = end - start + 1;
                (start, end, length)
            })
            .filter(|v| v.2 >= size);

        blocks.next().map(|v| (v.0, v.0 + size - 1))
    }
}

pub fn part1(input: &str) -> u64 {
    let mut disk = Disk::from_string(input.trim());

    println!("Disk has {} files", disk.num_files);

    disk.reorder();

    disk.checksum()
}

pub fn part2(input: &str) -> u64 {
    let mut disk = Disk::from_string(input.trim());

    println!("Disk has {} files", disk.num_files);

    disk.reorder_whole_files(true);

    disk.checksum()
}

fn slice_swap<T>(left: &mut [T], right: &mut [T]) {
    assert_eq!(left.len(), right.len());

    for (l, r) in left.iter_mut().zip(right) {
        std::mem::swap(l, r);
    }
}
