use std::collections::VecDeque;
use std::iter;

fn simulate(fish_states: &Vec<usize>, days: usize) -> u64 {
    let mut state_frequencies: VecDeque<u64> = iter::repeat(0).take(9).collect();

    for state in fish_states {
        state_frequencies[*state] += 1;
    }

    for _d in 0..days {
        let birthing = state_frequencies.pop_front().unwrap();
        state_frequencies.push_back(birthing);
        state_frequencies[6] += birthing;
    }

    return state_frequencies.iter().sum::<u64>();
}

fn main() {
    let input: Vec<usize> = vec![
        1, 1, 1, 1, 1, 1, 1, 4, 1, 2, 1, 1, 4, 1, 1, 1, 5, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 5,
        1, 1, 1, 1, 3, 1, 1, 2, 1, 2, 1, 3, 3, 4, 1, 4, 1, 1, 3, 1, 1, 5, 1, 1, 1, 1, 4, 1, 1, 5,
        1, 1, 1, 4, 1, 5, 1, 1, 1, 3, 1, 1, 5, 3, 1, 1, 1, 1, 1, 4, 1, 1, 1, 1, 1, 2, 4, 1, 1, 1,
        1, 4, 1, 2, 2, 1, 1, 1, 3, 1, 2, 5, 1, 4, 1, 1, 1, 3, 1, 1, 4, 1, 1, 1, 1, 1, 1, 1, 4, 1,
        1, 4, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 5, 1, 1, 1, 4, 1, 1, 5, 1, 1, 5, 3, 3, 5, 3, 1, 1, 1,
        4, 1, 1, 1, 1, 1, 1, 5, 3, 1, 2, 1, 1, 1, 4, 1, 3, 1, 5, 1, 1, 2, 1, 1, 1, 1, 1, 5, 1, 1,
        1, 1, 1, 2, 1, 1, 1, 1, 4, 3, 2, 1, 2, 4, 1, 3, 1, 5, 1, 2, 1, 4, 1, 1, 1, 1, 1, 3, 1, 4,
        1, 1, 1, 1, 3, 1, 3, 3, 1, 4, 3, 4, 1, 1, 1, 1, 5, 1, 3, 3, 2, 5, 3, 1, 1, 3, 1, 3, 1, 1,
        1, 1, 4, 1, 1, 1, 1, 3, 1, 5, 1, 1, 1, 4, 4, 1, 1, 5, 5, 2, 4, 5, 1, 1, 1, 1, 5, 1, 1, 2,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 5, 1, 1, 1, 1, 1, 1, 3, 1, 1, 2, 1, 1,
    ];

    println!("day 06 part 1 {}", simulate(&input, 80));
    println!("day 06 part 2 {}", simulate(&input, 256));
}
