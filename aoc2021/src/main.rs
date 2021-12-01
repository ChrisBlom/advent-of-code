use std::collections::VecDeque;
use std::fs::File;
use std::io::{BufRead, BufReader, Error};

fn increasing_windows(window_size: usize) -> Result<u32, Error> {
    let path = "inputs/day01.txt";
    let input = File::open(path)?;
    let buffered = BufReader::new(input);

    let mut window = VecDeque::with_capacity(3);
    let mut prev: i32 = i32::max_value();
    let mut count: u32 = 0;
    let mut window_sum: i32 = 0;

    for line in buffered.lines() {
        let num: i32 = line?.parse::<i32>().unwrap();

        window_sum = window_sum + num;
        window.push_back(num);
        if window.len() > window_size {
            window_sum = window_sum - window.pop_front().unwrap();
        }

        if window.len() == window_size {
            if window_sum > prev {
                //println!("window {}", windowSum);
                count = count + 1;
            }

            prev = window_sum;
        }
    }
    return Ok(count);
}

fn main() -> Result<(), Error> {
    println!("part1 {}", increasing_windows(1).unwrap());
    println!("part2 {}", increasing_windows(3).unwrap());
    Ok(())
}
