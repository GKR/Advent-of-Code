use std::fs;
use std::collections::HashSet;
use vector2d::Vector2D;

fn to_unit_vector(cmd: String) -> Vector2D<f32> {
    if cmd == "U" {
        return Vector2D { x: 0.0, y: 1.0 };
    } else if cmd == "D" {
        return Vector2D { x: 0.0, y: -1.0 };
    } else if cmd == "R" {
        return Vector2D { x: 1.0, y: 0.0 };
    } else if cmd == "L" {
        return Vector2D { x: -1.0, y: 0.0 };
    }
    return Vector2D { x: 0.0, y: 0.0 };
}

fn to_moves(cmd: Vec<&str>) -> Vec<Vector2D<f32>> {
    let amount = cmd[1].parse::<i32>().unwrap();
    let moves: Vec<Vector2D<f32>> = (0..amount).collect::<Vec<i32>>().into_iter().map(|_i| return to_unit_vector(cmd[0].to_string()) ).collect();
    return moves;
}

fn isclose(a: f32, b: f32, epsilon: f32) -> bool {
    (a - b).abs() <= a.abs().max(b.abs()) * epsilon
}

fn get_moves(rope_length: usize, lines: Vec<String>) -> usize {
    let mut rope: Vec<Vector2D<f32>> = (0..rope_length as i32).collect::<Vec<i32>>().into_iter().map(|_i| Vector2D { x: 0.0, y: 0.0 } ).collect();

    let mut tail_moves = HashSet::new();
    tail_moves.insert(format!("{},{}", rope[rope.len() - 1].x as i32, rope[rope.len() - 1].y as i32));

    for i in 0..lines.len() {
        let cmd: Vec<&str> = lines[i].split(' ').collect();
        let moves = to_moves(cmd);

        for i in 0..moves.len() {
            let v_move = moves[i];

            rope[0] += v_move; // Move head

            for j in 0..rope_length - 1 {
                let v_delta = rope[j] - rope[j + 1];
                let v_delta_move = Vector2D { 
                    x: if v_delta.x > 1.0 { 1.0 } else if v_delta.x < -1.0 { -1.0 } else { v_delta.x }, 
                    y: if v_delta.y > 1.0 { 1.0 } else if v_delta.y < -1.0 { -1.0 } else { v_delta.y }
                };
                let length = (rope[j] - rope[j + 1]).length();

                if isclose(length, 2.0, 1.0e-9) || length > 2.0 {
                    rope[j + 1] += v_delta_move;
                }

                if j + 1 == rope_length - 1 {
                    tail_moves.insert(format!("{},{}", rope[j + 1].x as i32, rope[j + 1].y as i32));
                }
            }
        }
    }

    return tail_moves.len();
}

fn main() {
    let file_contents = fs::read_to_string("input.txt").unwrap();
    let mut lines = Vec::new();
    for line in file_contents.lines() {
        lines.push(line.to_string());
    }

    println!("Moves for rope size 2: {}", get_moves(2, lines.clone()));
    println!("Moves for rope size 10: {}", get_moves(10, lines.clone()));
}
