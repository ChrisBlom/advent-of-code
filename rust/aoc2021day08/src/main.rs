use std::array::IntoIter;
use std::collections::VecDeque;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{BufRead, BufReader, Error};
use std::iter::FromIterator;

fn main() -> Result<(), Error> {
    let hashmap: HashMap<usize, &str> = HashMap::from_iter(IntoIter::new([
        (0, "abcdefg"),
        (1, "cf"),
        (2, "acdeg"),
        (3, "acdfg"),
        (4, "bcdf"),
        (5, "abdfg"),
        (6, "abdefg"),
        (7, "acf"),
        (8, "abcefg"),
        (9, "abcdfg"),
    ]));

    let mut by_length = HashMap::new();

    for (k, v) in hashmap {
        by_length
            .entry(v.len())
            .or_insert_with(|| HashSet::new())
            .insert(k);
    }

    let uniques: HashSet<&usize> = by_length
        .iter()
        .filter(|(count, digits)| digits.len() == 1)
        .map(|(count, digits)| count)
        .collect();

    println!("{:?}", by_length);
    println!("{:?}", uniques);

    let path = "input.txt";
    let input = File::open(path)?;
    let buffered = BufReader::new(input);

    let mut sum: usize = 0;
    for line in buffered.lines() {
        let l = line.unwrap();
        let parts: Vec<&str> = l.split("|").collect();

        let words: usize = parts[1]
            .split_whitespace()
            .filter(|word| uniques.contains(&word.len()))
            .count();

        sum += words;
    }
    println!("{:?}", sum);
    Ok(())
}
