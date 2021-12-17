use std::cmp::max;

fn step((x, y, dx, dy): (i32, i32, i32, i32)) -> (i32, i32, i32, i32) {
    return (x + dx, y + dy, dx - dx.signum(), dy - 1);
}

fn in_target(
    (x, y, _, _): (i32, i32, i32, i32),
    (minx, maxx, miny, maxy): (i32, i32, i32, i32),
) -> bool {
    return x >= minx && x <= maxx && y >= miny && y <= maxy;
}

fn missed(
    (x, y, _, _): (i32, i32, i32, i32),
    (_minx, maxx, miny, _maxy): (i32, i32, i32, i32),
) -> bool {
    return y < miny || x > maxx;
}

fn max_y(state: (i32, i32, i32, i32), target: (i32, i32, i32, i32)) -> Option<i32> {
    let mut s = state;
    let mut max_y = 0;

    while !missed(s, target) {
        if in_target(s, target) {
            return Some(max_y);
        }
        s = step(s);
        max_y = max(s.1, max_y);
    }
    None
}

fn main() {
    //let t = (20, 30, -10, -5);
    let t = (32, 65, -225, -177);

    let mut highest = 0;
    let mut angles = 0;
    for dx in 0..1000 {
        for dy in -1000..1000 {
            let s = (0, 0, dx, dy);
            match max_y(s, t) {
                Some(y) => {
                    highest = max(y, highest);
                    angles += 1;
                    // println!("dx={} dy={} {:?}", dx, dy, y)
                }
                None => (),
            }
        }
    }

    println!("part 1: {}", highest);
    println!("part 2: {}", angles);
}
