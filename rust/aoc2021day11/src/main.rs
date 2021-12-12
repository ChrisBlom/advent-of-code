use std::collections::{HashSet, VecDeque};
use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

fn update(grid: &mut Vec<Vec<(u32, bool)>>, y: usize, x: usize) {
    let h = grid.len() as i32;
    let w = grid[0].len() as i32;

    let d: Vec<i32> = vec![-1, 0, 1];

    if grid[y][x].0 > 9 && !grid[y][x].1 {
        grid[y][x].1 = true;
        for dy in &d {
            for dx in &d {
                if !(*dx == 0 && *dy == 0) {
                    let cy = y as i32 + dy;
                    let cx = x as i32 + dx;
                    if cy >= 0 && cy < h && cx >= 0 && cx < w {
                        grid[cy as usize][cx as usize].0 += 1;
                        update(grid, cy as usize, cx as usize);
                    }
                }
            }
        }
    }
}

// fn step(grid: &mut Vec<Vec<u32>>) {
//     let h = grid.len() as i32;
//     let w = grid[0].len() as i32;

//     let mut flashed: HashSet<(usize, usize)> = HashSet::new();
//     let mut todo: VecDeque<(usize, usize)> = VecDeque::new();

//     for y in 0..grid.len() {
//         for x in 0..grid[0].len() {
//             grid[y][x] += 1;
//             if grid[y][x] > 9 {
//                 todo.push_front((y, x));
//                 flashed.insert((y, x));
//             }
//         }
//     }

//     let d: Vec<i32> = vec![-1, 0, 1];

//     while !todo.is_empty() {
//         let (y, x) = todo.pop_front().unwrap();
//         //        println!("flash {:?}", (y, x));
//         for dy in &d {
//             for dx in &d {
//                 if !(*dx == 0 && *dy == 0) {
//                     let cy = y as i32 + dy;
//                     let cx = x as i32 + dx;
//                     if cy >= 0 && cy < h && cx >= 0 && cx < w {
//                         let cyi = cy as usize;
//                         let cxi = cx as usize;

//                         grid[cyi][cxi].0 += 1;
//                         if grid[cyi][cxi] > 9 && !flashed.contains(&(cyi, cyi)) {
//                             todo.push_front((cyi, cxi));
//                             flashed.insert((cyi, cyi));
//                         }
//                     }
//                 }
//             }
//         }
//     }

//     for (y, x) in flashed {
//         grid[y][x] = 0;
//     }
// }

fn main() {
    //     let input = "11111
    // 19991
    // 19191
    // 19991
    // 11111";
    //actual input
    let input = "6636827465
6774248431
4227386366
7447452613
6223122545
2814388766
6615551144
4836235836
5334783256
4128344843";

    //     // example
    //     let input = "5483143223
    // 2745854711
    // 5264556173
    // 6141336146
    // 6357385478
    // 4167524645
    // 2176841721
    // 6882881134
    // 4846848554
    // 5283751526";

    let f = input.as_bytes();
    //    let f = File::open("input.txt").unwrap();
    let mut lines = BufReader::new(f).lines();

    let mut grid = lines
        .map(|l| {
            l.unwrap()
                .chars()
                .map(|c| (c.to_digit(10).unwrap(), false))
                .collect()
        })
        .collect::<Vec<Vec<(u32, bool)>>>();
    let h = grid.len() as usize;
    let w = grid[0].len() as usize;

    //    step(&mut grid);

    for l in &grid {
        println!["{:?}", l.iter().map(|c| c.0).collect::<Vec<u32>>()];
    }

    let mut flashes: usize = 0;
    let mut prev_flashes: usize = 0;

    for i in 0..1000 {
        for y in 0..grid.len() {
            for x in 0..grid[0].len() {
                grid[y][x].0 += 1;
            }
        }

        // println!["after inc:"];
        // for l in &grid {
        //     println!["{:?}", l.iter().map(|c| c.0).collect::<Vec<u32>>()];
        // }

        let d: Vec<i32> = vec![-1, 0, 1];

        let mut some_flashes = true;

        while some_flashes {
            some_flashes = false;
            for y in 0..grid.len() {
                for x in 0..grid[0].len() {
                    if grid[y][x].0 > 9 && !grid[y][x].1 {
                        grid[y][x].1 = true;
                        flashes += 1;
                        some_flashes = true;
                        for dy in &d {
                            for dx in &d {
                                if !(*dx == 0 && *dy == 0) {
                                    let cy = y as i32 + dy;
                                    let cx = x as i32 + dx;
                                    if cy >= 0 && cy < h as i32 && cx >= 0 && cx < w as i32 {
                                        grid[cy as usize][cx as usize].0 += 1;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        if flashes - prev_flashes == w * h {
            println!("SYNC! {}", i + 1);
            break;
        };

        for y in 0..grid.len() {
            for x in 0..grid[0].len() {
                if grid[y][x].1 {
                    grid[y][x].0 = 0;
                    grid[y][x].1 = false;
                }
            }
        }

        println!["- after reset {}: ", i + 1,];
        for l in &grid {
            println!["{:?}", l.iter().map(|c| c.0).collect::<Vec<u32>>()];
        }

        prev_flashes = flashes;
    }
    println!["-- {} ", flashes];
}
