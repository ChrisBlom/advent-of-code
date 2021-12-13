use std::collections::{HashMap, HashSet};
use std::io::{BufRead, BufReader, Error};
use std::{self, fs::File};

const EXAMPLE: &str = "2199943210
3987894921
9856789892
8767896789
9899965678";

fn basin_size(
    grid: &Vec<Vec<i32>>,
    visited: &mut HashSet<(usize, usize)>,
    pos: (usize, usize),
) -> usize {
    let h = grid.len();
    let w = grid[0].len();

    let (y, x) = pos;

    if grid[y][x] == 9 || visited.contains(&pos) {
        return 0;
    }

    visited.insert((y, x));

    return 1
        + if y < h - 1 { basin_size(grid,visited, (y + 1, x) ) } else { 0 } // north
        + if y > 0 { basin_size(grid,visited, (y - 1, x)) } else { 0 } // south
        + if x < w - 1 { basin_size(grid,visited, (y, x + 1)) } else { 0 } // east
        + if x > 0 { basin_size(grid,visited, (y, x - 1)) } else { 0 } // west
    ;
}

fn main() -> Result<(), Error> {
    let input = File::open("input.txt")?;

    //let input = EXAMPLE.as_bytes();

    let buffered = BufReader::new(input);

    let grid = buffered
        .lines()
        .map(|l| {
            l.unwrap()
                .chars()
                .map(|c| c.to_string().parse::<i32>().unwrap())
                .collect::<Vec<i32>>()
        })
        .collect::<Vec<Vec<i32>>>();

    let mut low_points = vec![];
    let mut low_points_locs: Vec<(usize, usize)> = vec![];

    for y in 0..grid.len() {
        for x in 0..grid[y].len() {
            let middle = grid[y][x];

            let mut min = middle;
            for dy in 0..3 {
                for dx in 0..3 {
                    let cx: i32 = x as i32 + dx - 1;
                    let cy: i32 = y as i32 + dy - 1;

                    if cx >= 0
                        && cy >= 0
                        && (cy as usize) < grid.len()
                        && (cx as usize) < grid[(cy as usize)].len()
                    {
                        let neigbour_val = grid[cy as usize][cx as usize];
                        if neigbour_val < min {
                            min = neigbour_val;
                        }
                    }
                }
            }

            if min == middle {
                low_points.push(min);
                low_points_locs.push((y, x));
            }
        }
    }

    println!("{:?}", low_points);

    println!(
        "part 1: {:?}",
        low_points.iter().map(|x| x + 1).sum::<i32>()
    );

    let mut visited = HashSet::new();

    let mut basins = low_points_locs
        .into_iter()
        .map(|pos| basin_size(&grid, &mut visited, pos))
        .collect::<Vec<usize>>();

    basins.sort_by(|a, b| b.cmp(a));

    let part2: usize = basins.iter().take(3).product();

    println!("part 2: {:?}", part2);

    return Ok(());
}
