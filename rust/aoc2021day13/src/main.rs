use std::{
    collections::HashSet,
    fs::File,
    io::{BufRead, BufReader},
};

fn parse_fold(str: String) -> (String, i32) {
    let parts = &mut str[("fold along ".len())..].split("=");
    return (
        parts.next().unwrap().to_string(),
        parts.next().unwrap().parse::<i32>().unwrap(),
    );
}

fn parse_coord(line: String) -> (i32, i32) {
    let mut parts = line.split(",").map(|c| c.parse::<i32>().unwrap());
    return (parts.next().unwrap(), parts.next().unwrap());
}

fn main() {
    let mut coords = BufReader::new(File::open("input.txt").unwrap())
        .lines()
        .take_while(|l| -> bool { !l.as_ref().unwrap().is_empty() })
        .map(|l| parse_coord(l.unwrap()))
        .collect::<HashSet<(i32, i32)>>();

    let folds: Vec<(String, i32)> = BufReader::new(File::open("input.txt").unwrap())
        .lines()
        .skip(coords.len() + 1)
        .map(|l| parse_fold(l.unwrap()))
        .collect();

    let mut i = 0;
    for f in folds {
        coords = coords.iter().map(|xy| fold_coord(f.clone(), *xy)).collect();
        if i == 0 {
            println!("part 1: {:?}", coords.len())
        };
        i = i + 1;
    }

    println!("part 2:");
    for y in 0..6 {
        for x in 0..40 {
            let c = if coords.contains(&(x, y)) { "█" } else { " " };
            print!("{}", c);
        }
        println!();
    }
}

fn reflect(point: i32, reflect_around: i32) -> i32 {
    return if point > reflect_around {
        reflect_around - (point - reflect_around)
    } else {
        point
    };
}

fn fold_coord(fold: (String, i32), coord: (i32, i32)) -> (i32, i32) {
    return match fold.0.as_str() {
        "x" => (reflect(coord.0, fold.1), coord.1),
        "y" => (coord.0, reflect(coord.1, fold.1)),
        _ => panic![],
    };
}
