use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

fn is_open(c: char) -> bool {
    match c {
        '{' => true,
        '[' => true,
        '(' => true,
        '<' => true,
        _ => false,
    }
}

fn close(c: char) -> char {
    match c {
        '{' => '}',
        '[' => ']',
        '(' => ')',
        '<' => '>',
        _ => panic!["unsupported char"],
    }
}

#[derive(Debug)]
enum Line {
    Valid,
    Corrupt(char),
    NotComplete(Vec<char>),
}

fn process(line: &str) -> Line {
    let mut stack: Vec<char> = vec![];

    for i in line.chars() {
        //        println!("{}", i);

        if is_open(i) {
            stack.push(i)
        } else {
            match stack.last() {
                Some(x) => {
                    if i != close(*x) {
                        return Line::Corrupt(i);
                    } else {
                        stack.pop();
                    }
                }
                None => {
                    panic!["unsupported char"]
                }
            }
        }
    }

    if stack.is_empty() {
        return Line::Valid;
    } else {
        return Line::NotComplete(stack);
    }
}

fn score_part1(c: char) -> usize {
    match c {
        ')' => 3,
        ']' => 57,
        '}' => 1197,
        '>' => 25137,
        _ => panic!["unsupported char"],
    }
}

fn score_part2(c: char) -> usize {
    match c {
        ')' => 1,
        ']' => 2,
        '}' => 3,
        '>' => 4,
        _ => panic!["unsupported char '{}; ", c],
    }
}

fn score_completion(stack: Vec<char>) -> usize {
    let mut s = 0;
    let mut stck = stack.clone();

    while !stck.is_empty() {
        s = s * 5 + score_part2(close(stck.pop().unwrap()))
    }
    return s;
}

fn main() {
    let ex = "{()()()]";

    let f = File::open("input.txt").unwrap();
    let mut lines = BufReader::new(f).lines();

    let mut part1: usize = 0;

    // for line in lines {
    //     let l = &line.unwrap();
    //     println!("Hello, world! {:?} ", process(l));
    //     part1 += match process(l) {
    //         Line::Corrupt(c) => score_part1(c),
    //         _ => 0,
    //     };
    // }

    let mut p2 = lines
        .map(|l| process(&l.unwrap()))
        .flat_map(|l| match l {
            Line::NotComplete(s) => Some(score_completion(s)),
            _ => None,
        })
        .collect::<Vec<usize>>();

    p2.sort();

    let middle = p2.len() / 2;
    println!("part2 {}", p2[middle])
}
