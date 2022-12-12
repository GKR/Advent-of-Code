use std::fs;
use pathfinding::prelude::astar;

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
struct Pos(i32, i32);

impl Pos {
    fn distance(&self, other: &Pos) -> usize {
        ((self.0 - other.0).abs() + (self.1 - other.1).abs()) as usize
    }

    fn neighbours(&self, grid: &Vec<Vec<char>>) -> Vec<(Pos, usize)> {
        let &Pos(x, y) = self;
        let current_char = grid[y as usize][x as usize];
        let mut neighbours: Vec<Pos> = vec![];

        if is_valid_neighbour(grid, Pos(x, y), Pos(x - 1, y)) {
            neighbours.push(Pos(x - 1, y));
        }
        if is_valid_neighbour(grid, Pos(x, y), Pos(x + 1, y)) {
            neighbours.push(Pos(x + 1, y));
        }
        if is_valid_neighbour(grid, Pos(x, y), Pos(x, y - 1)) {
            neighbours.push(Pos(x, y - 1));
        }
        if is_valid_neighbour(grid, Pos(x, y), Pos(x, y + 1)) {
            neighbours.push(Pos(x, y + 1));
        }

        let debug = false;
        if debug {
            println!("neighbours, current_char: {}, [{}, {}]", current_char, x, y);
            for gy in 0..grid.len() {
                for gx in 0..grid[gy].len() {
                    if gx == x as usize && gy == y as usize {
                        print!(" {} ", grid[gy][gx]);    
                    } else {
                        print!("[{}]", grid[gy][gx]);
                    }
                }
                println!("");
            }
        }

        return neighbours.into_iter().map(|p| (p, 1)).collect();
    }
}

fn can_step_up(current: char, next: char) -> bool {
    if next == 'S' {
        return false;
    }

    if current == 'S' && (next == 'a' || next == 'b') {
        return true;
    }

    if (current == 'y' || current == 'z') && next == 'E' {
        return true;
    }

    let diff = next as i32 - current as i32;
    return diff <= 1;
}

fn is_valid_neighbour(grid: &Vec<Vec<char>>, pos: Pos, next: Pos) -> bool {
    let current_char = grid[pos.1 as usize][pos.0 as usize];
    if next.0 >= 0 && next.0 < grid[0].len() as i32 && next.1 >= 0 && next.1 < grid.len() as i32 {
        return can_step_up(current_char, grid[next.1 as usize][next.0 as usize]);
    }

    return false;
}

fn main() {
    let file_contents = fs::read_to_string("input.txt").unwrap();
    let mut lines = Vec::new();
    for line in file_contents.lines() {
        lines.push(line.to_string());
    }

    let mut grid: Vec<Vec<char>> = vec![];
    let mut goal = Pos(0, 0);
    let mut start = Pos(0, 0);
    let mut start_positions: Vec<Pos> = vec![];

    for y in 0..lines.len() {
        grid.push(vec![]);
        for (x, c) in lines[y].chars().enumerate() {
            grid[y].push(c);
            if c == 'E' {
                goal = Pos(x as i32, y as i32);
            } else if c == 'S' {
                start = Pos(x as i32, y as i32);
            } else if c == 'a' {
                start_positions.push(Pos(x as i32, y as i32));
            }
        }
    }

    let debug = false;
    if debug {
        for y in 0..grid.len() {
            for x in 0..grid[y].len() {
                print!("[{}]", grid[y][x]);
            }
            println!("");
        }
        println!("{}x{}", grid.len(), grid[0].len());
        println!("goal: {}x{}", goal.0, goal.1);
        println!("[{}]", 'b' as u32 - 'a' as u32);
    }

    let result = astar(&start, |p| p.neighbours(&grid), |p| p.distance(&goal), |p| *p == goal);

    if let Some(results) = result {
        println!("Part 1: {}", results.1);
    }
    
    let mut possible_path_lengths = vec![];
    for start_position in start_positions {
        let part2_result = astar(&start_position, |p| p.neighbours(&grid), |p| p.distance(&goal), |p| *p == goal);
        if let Some(part2_results) = part2_result {
            possible_path_lengths.push(part2_results.1);
        }
    }
    possible_path_lengths.sort();

    println!("Part 2: {}", possible_path_lengths[0]);
}
