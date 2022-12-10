use std::fs;
use std::collections::HashSet;

fn get_cycle_marker(cycle: i32) -> i32 {
    let array: Vec<i32> = vec![cycle - 20, cycle + 1, cycle + 2];
    let result = array.iter().filter(|&&x| (x - 20) % 40 == 0).cloned().collect::<Vec<i32>>();
    
    if result.len() > 0 {
        return result[0];
    }

    return 0;
}

fn print_lcd(pixels: Vec<char>) {
    for row in 0..6 {
        for col in 0..40 {
            print!("{}", pixels[(row * 40) + col]);
        }
        println!("");
    }
}

fn update_pixels(pixels: &mut Vec<char>, position: i32, sprite_position: i32) {
    if position >= sprite_position - 1 && position <= sprite_position + 1 {
        if position >= 0 && position < pixels.len() as i32 {
            pixels[position as usize] = '#';
        }
    }
}

fn main() {
    let file_contents = fs::read_to_string("input.txt").unwrap();
    let mut lines = Vec::new();
    for line in file_contents.lines() {
        lines.push(line.to_string());
    }

    let mut cycle = 0;
    let mut register_x = 1;
    let mut signal_strength_sum = 0;
    let mut markers = HashSet::new();
    let mut pixels = vec!['.'; 40 * 6];
    
    for i in 0..lines.len() {
        let cmd: Vec<&str> = lines[i].split(' ').collect();

        let cycle_marker = get_cycle_marker(cycle);

        if cycle_marker > 0 && !markers.contains(&cycle_marker) {
            signal_strength_sum += cycle_marker * register_x;
            markers.insert(cycle_marker);
        }

        if cmd[0] == "noop" {
            update_pixels(&mut pixels, cycle, register_x + (40 * (cycle / 40)));
            cycle += 1;
        } else if cmd[0] == "addx" {
            update_pixels(&mut pixels, cycle, register_x + (40 * (cycle / 40)));
            cycle += 1;

            update_pixels(&mut pixels, cycle, register_x + (40 * (cycle / 40)));
            cycle += 1;

            let value = cmd[1].parse::<i32>().unwrap();
            register_x += value;
        }
    }

    println!("signal_strength_sum: {}", signal_strength_sum);
    print_lcd(pixels.clone());
}
