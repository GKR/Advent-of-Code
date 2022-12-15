use std::fs;
use vector2d::Vector2D;

struct LineTo {
    from: Vector2D<i32>,
    to: Vector2D<i32>,
}

#[derive(PartialEq)]
enum SimulationState {
    NoChange,
    Falling,
    Resting,
    OutOfBounds,
}

struct SimulationStep {
    state: SimulationState,
    pos: Vector2D<i32>,
}

fn draw_grid(grid: &Vec<Vec<char>>) {
    for y in 0..grid.len() {
        for x in 0..grid[y].len() {
            print!("{}", grid[y][x]);
        }
        println!("");
    }
}

fn get_char_at(grid: &mut Vec<Vec<char>>, x: i32, y: i32) -> char {
    if x > 0 && y > 0 && x < grid[0].len() as i32 && y < grid.len() as i32 {
        return grid[y as usize][x as usize];
    }
    return ' ';
}

fn simulate_step(grid: &mut Vec<Vec<char>>, pos: Vector2D<i32>) -> SimulationStep {
    if pos.y as usize >= grid.len() - 1 {
        return SimulationStep { state: SimulationState::OutOfBounds, pos: Vector2D::new(0, 0) };
    }
    if pos.x as usize >= grid[0].len() {
        return SimulationStep { state: SimulationState::OutOfBounds, pos: Vector2D::new(0, 0) };
    }

    let char_below = get_char_at(grid, pos.x, pos.y + 1);
    let char_right = get_char_at(grid, pos.x + 1, pos.y + 1);
    let char_left = get_char_at(grid, pos.x - 1, pos.y + 1);

    if char_below == '.' {
        return SimulationStep { state: SimulationState::Falling, pos: Vector2D::new(pos.x, pos.y + 1) };
    }

    if char_below == '#' || char_below == 'o' {
        let can_fall_right = char_right == '.';
        let can_fall_left = char_left == '.';
        if can_fall_left {
            return SimulationStep { state: SimulationState::Falling, pos: Vector2D::new(pos.x - 1, pos.y + 1) };
        }
        else if can_fall_right {
            return SimulationStep { state: SimulationState::Falling, pos: Vector2D::new(pos.x + 1, pos.y + 1) };
        }
        else {
            return SimulationStep { state: SimulationState::Resting, pos: Vector2D::new(pos.x, pos.y) };
        }
    }

    return SimulationStep { state: SimulationState::NoChange, pos: Vector2D::new(0, 0) };
}

fn simulate(grid: &mut Vec<Vec<char>>, pos: Vector2D<i32>) -> SimulationStep {
    let mut grain_pos = pos;
    let mut run = true;
    let mut step: SimulationStep = SimulationStep { state: SimulationState::NoChange, pos: Vector2D::new(0, 0) };

    while run {
        let mut step = simulate_step(grid, grain_pos);

        if step.state == SimulationState::OutOfBounds {
            run = false;
            return step;
        } else if step.state == SimulationState::Resting {
            run = false;
            grid[step.pos.y as usize][step.pos.x as usize] = 'o';
            return step;
        } else if step.state == SimulationState::NoChange {
            run = false;
        } else {
            grain_pos = step.pos;
        }
    }

    return step;
}

fn main() {
    let file_contents = fs::read_to_string("input.txt").unwrap();
    let mut lines: Vec<LineTo> = Vec::new();
    let mut max_bounds: Vector2D<i32> = Vector2D { x: 0, y: 0 };
    let mut grid: Vec<Vec<char>> = vec![];

    let mut start: Vector2D<i32> = Vector2D { x: 500, y: 0 };
    
    for line in file_contents.lines() {
        let line_str = line.to_string();
        let split = line_str.split("->");
        let vec: Vec<&str> = split.collect();
        let mut prev_vector: Vector2D<i32> = Vector2D { x: 0, y: 0 };
        
        for (i, v) in vec.iter().enumerate() {
            let vector_str = v.trim();
            let vector_split: Vec<&str> = vector_str.split(",").collect();
            let x = vector_split[0].parse::<i32>().unwrap();
            let y = vector_split[1].parse::<i32>().unwrap();
            let vector = Vector2D { x: x, y: y };
            
            if i > 0 {
                lines.push(LineTo { from: prev_vector, to: vector });
            }
            prev_vector = vector;

            if x > max_bounds.x {
                max_bounds.x = x + 1;
            }
            if y > max_bounds.y {
                max_bounds.y = y + 1;
            }
        }
    }

    max_bounds.x = max_bounds.x * 2; // "Infinite floor" :)

    for y in 0..max_bounds.y {
        grid.push(vec![]);
        for x in 0..max_bounds.x {
            grid[y as usize].push('.');
        }
    }

    // Draw lines
    for (i, line_to) in lines.iter().enumerate() {
        let mut delta = line_to.to - line_to.from;
        while delta.x != 0 || delta.y != 0 {
            let point = line_to.from + delta;
            if (point.y as usize) < grid.len() && (point.x as usize) < grid[0].len() {
                grid[point.y as usize][point.x as usize] = '#';
            }
            

            if delta.x != 0 {
                delta.x -= delta.x.signum();
            }
            if delta.y != 0 {
                delta.y -= delta.y.signum();
            }
        }
        grid[line_to.from.y as usize][line_to.from.x as usize] = '#';
    }

    { // Part 1
        let mut counter = 0;
        let mut grains = 0;
        let mut cloned_grid = grid.clone();
        while counter < 1000 {
            let step = simulate(&mut cloned_grid, start);
            if step.state == SimulationState::Resting {
                grains += 1;
            }
            counter += 1;
        }
        println!("Part 1: {}", grains);
    }

    // Part 2
    {
        // Add new floor for part 2
        grid.push(vec![]);
        for x in 0..max_bounds.x {
            grid[max_bounds.y as usize].push('.');
        }
        grid.push(vec![]);
        for x in 0..max_bounds.x {
            grid[max_bounds.y as usize + 1].push('#');
        }
    
        let mut counter = 0;
        let mut grains = 0;
        let mut cloned_grid = grid.clone();
        while counter < 30000 {
            let step = simulate(&mut cloned_grid, start);
            if step.state == SimulationState::Resting {
                grains += 1;
                if step.pos.x == 500 && step.pos.y == 0 {
                    break;
                }
            }
            counter += 1;
        }
        println!("Part 2: {}", grains);
    }

    //draw_grid(&grid);
}
