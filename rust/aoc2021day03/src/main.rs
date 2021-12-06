use std::collections::HashMap;
use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

fn most_frequent_bit(lines: &Vec<String>, candidates: &HashSet<usize>, col: usize) -> Option<char> {
    let mut count0 = 0;
    let mut count1 = 0;

    for row_i in candidates {
        if lines[*row_i].chars().nth(col).unwrap() == '0' {
            count0 += 1;
        } else {
            count1 += 1;
        }
    }

    if count0 > count1 {
        return Some('0');
    } else if count1 > count0 {
        return Some('1');
    } else {
        return None;
    }
}

fn leastfrequent_bit(lines: &Vec<String>, candidates: &HashSet<usize>, col: usize) -> Option<char> {
    let mut count0 = 0;
    let mut count1 = 0;

    for row_i in candidates {
        if lines[*row_i].chars().nth(col).unwrap() == '0' {
            count0 += 1;
        } else {
            count1 += 1;
        }
    }

    if count0 < count1 {
        return Some('0');
    } else if count1 < count0 {
        return Some('1');
    } else {
        return None;
    }
}

fn selectwithbit(lines: &Vec<String>, candidates: &mut HashSet<usize>, col: usize, bit: char) {
    let mut to_remove: Vec<usize> = vec![];

    for i in candidates.iter() {
        if lines[*i].chars().nth(col).unwrap() != bit {
            to_remove.push(*i);
        }
    }
    for i in to_remove {
        candidates.remove(&i);
    }
}

fn oxygen(lines: &Vec<String>) -> usize {
    let mut col = 0;

    let mut candidates: HashSet<usize> = (0..lines.len()).collect();

    while candidates.len() > 1 {
        let mf = most_frequent_bit(&lines, &candidates, col);
        if mf.is_some() {
            selectwithbit(&lines, &mut candidates, col, mf.unwrap())
        } else {
            selectwithbit(&lines, &mut candidates, col, '1')
        }
        col += 1;
    }

    let last = &lines[*candidates.iter().next().unwrap()];

    return usize::from_str_radix(last.as_str(), 2).unwrap();
}

fn co2(lines: &Vec<String>) -> usize {
    let mut candidates: HashSet<usize> = (0..lines.len()).collect();

    let mut col = 0;
    while candidates.len() > 1 {
        let mf = leastfrequent_bit(&lines, &candidates, col);
        if mf.is_some() {
            selectwithbit(&lines, &mut candidates, col, mf.unwrap())
        } else {
            selectwithbit(&lines, &mut candidates, col, '0')
        }
        col += 1;
    }

    let last = &lines[*candidates.iter().next().unwrap()];

    return usize::from_str_radix(last.as_str(), 2).unwrap();
}

fn main() {
    let f = File::open("input.txt").unwrap();
    let r = BufReader::new(f);
    let lines: Vec<String> = r.lines().map(|x| x.unwrap()).collect();
    println!("day03 part 2 {}", oxygen(&lines) * co2(&lines));
}
