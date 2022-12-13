use std::fs;
use std::cmp::Ordering;
use serde_json::{json, Value};

fn make_array(value: &Value) -> Vec<Value> {
    if value.is_array() {
        return value.as_array().unwrap().to_vec();
    } else {
        return vec![value.clone()];
    }
}

fn compare(left: &Vec<Value>, right: &Vec<Value>) -> i32 {
    for (i, r_value) in right.iter().enumerate() {
        if i >= left.len() {
            return -1;
        }

        let l_value = &left[i];

        if l_value.is_array() || r_value.is_array() {
            let compare_result = compare(&make_array(l_value), &make_array(r_value));
            if compare_result == 0 {
                continue;
            } else {
                return compare_result;
            }
        }

        let l_number = l_value.as_i64().unwrap();
        let r_number = r_value.as_i64().unwrap();

        if l_number == r_number {
            continue;
        } else if r_number > l_number {
            return -1;
        } else {
            return 1;
        }
    }

    if left.len() == right.len() {
        return 0;
    }

    if right.len() > left.len() {
        return -1
    } else {
        return 1
    }
}

fn main() {
    let file_contents = fs::read_to_string("input.txt").unwrap();
    let mut input_packets: Vec<String> = Vec::new();
    let mut all_packets: Vec<Value> = Vec::new();
    let mut pair_index = 0;
    let mut total = 0;

    for (i, line) in file_contents.lines().enumerate() {
        if input_packets.len() == 2 {
            pair_index += 1;
            let value1: Value = serde_json::from_str::<Value>(&input_packets[0]).unwrap();
            let value2: Value = serde_json::from_str::<Value>(&input_packets[1]).unwrap();

            all_packets.push(value1.clone());
            all_packets.push(value2.clone());

            let compare_result = compare(value1.as_array().unwrap(), &value2.as_array().unwrap());
            if compare_result == -1 {
                total += pair_index;
            }
            //println!("compare = {}", compare(value1.as_array().unwrap(), &value2.as_array().unwrap()));
            input_packets = vec![];
        } else if line.trim().len() > 0 {
            input_packets.push(line.to_string());
        }   
    }

    all_packets.push(json!([[2]]));
    all_packets.push(json!([[6]]));

    all_packets.sort_by(|packet_1, packet_2| {
        let compare_result = compare(packet_1.as_array().unwrap(), &packet_2.as_array().unwrap());
        if compare_result == -1 {
            return Ordering::Less;
        }
        return Ordering::Greater;
    });

    let mut index_2 = 0;
    let mut index_6 = 0;
    for i in 0..all_packets.len() {
        let packet = all_packets[i].as_array().unwrap();
        if packet.len() == 1 && packet[0].is_array() {
            let first_array = packet[0].as_array().unwrap();
            if first_array.len() == 1 && first_array[0].is_i64() {
                let first_value = first_array[0].as_i64().unwrap();
                if first_value == 2 {
                    index_2 = i + 1;
                }
                if first_value == 6 {
                    index_6 = i + 1;
                }
            }
        }
    }

    println!("part 1: {}", total);
    println!("part 2: {}", index_2 * index_6);
}
