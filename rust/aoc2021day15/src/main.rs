use std::{
    cmp::Ordering,
    collections::BinaryHeap,
    fs::File,
    io::{BufRead, BufReader},
};

fn example() -> String {
    return "1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
"
    .to_string();
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
struct State {
    cost: usize,
    pos: (usize, usize),
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other
            .cost
            .cmp(&self.cost)
            .then_with(|| self.pos.cmp(&other.pos))
    }
}

// `PartialOrd` needs to be implemented as well.
impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn wrap_risk_level(x: usize) -> usize {
    return if x > 9 { 1 } else { x };
}

fn repeat_grid_levels(grid: &Vec<Vec<usize>>, h: usize, w: usize, y: usize, x: usize) -> usize {
    if x < w && y < h {
        return grid[y][x];
    } else if x < w {
        return wrap_risk_level(repeat_grid_levels(grid, h, w, y - h, x) + 1);
    } else {
        return wrap_risk_level(repeat_grid_levels(grid, h, w, y, x - w) + 1);
    }
}

fn enlarge_grid(grid: &Vec<Vec<usize>>, n: usize) -> Vec<Vec<usize>> {
    let h = grid.len();
    let w = grid[0].len();

    return (0..h * n)
        .map(|y| {
            (0..w * n)
                .map(|x| repeat_grid_levels(&grid, h, w, y, x))
                .collect::<Vec<usize>>()
        })
        .collect::<_>();
}

fn shortest_path(grid: &Vec<Vec<usize>>) -> Option<usize> {
    let h = grid.len();
    let w = grid[0].len();

    let target_pos = (h - 1, w - 1);

    let mut dist = (0..grid.len())
        .map(|y| (0..grid[y].len()).map(|_x| usize::MAX).collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let mut visited = (0..grid.len())
        .map(|y| (0..grid[y].len()).map(|_x| false).collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let mut heap = BinaryHeap::new();

    dist[0][0] = 0;
    heap.push(State {
        cost: 0,
        pos: (0, 0),
    });

    while let Some(State { cost, pos }) = heap.pop() {
        let (y, x) = pos;

        if pos == target_pos {
            return Some(cost);
        }

        if cost > dist[y][x] {
            continue;
        }

        for (dy, dx) in vec![(1, 0), (0, 1), (-1, 0), (0, -1)] {
            let ny = y as i32 + dy;
            let nx = x as i32 + dx;
            if ny >= 0 && ny < h as i32 && nx >= 0 && nx < w as i32 {
                let ny = ny as usize;
                let nx = nx as usize;
                let next_cost = cost + grid[ny][nx];
                if next_cost < dist[ny][nx] && !visited[ny][nx] {
                    let next_pos = (ny, nx);
                    visited[ny][nx] = true;
                    heap.push(State {
                        cost: next_cost,
                        pos: next_pos,
                    });
                }
            }
        }
    }
    None
}

fn main() {
    let f = File::open("input.txt").unwrap();

    //let gridin = BufReader::new(example().as_bytes())
    let grid = BufReader::new(f)
        .lines()
        .map(|l| {
            l.unwrap()
                .chars()
                .map(|c| c.to_digit(10).unwrap() as usize)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    // for row in &grid {
    //     for cell in row {
    //         print!("{}", celn)
    //     };
    //     println!(n;
    // }

    println!("part 1: {:?}", shortest_path(&grid).unwrap());
    println!(
        "part 2: {:?}",
        shortest_path(&enlarge_grid(&grid, 5)).unwrap()
    )
}
