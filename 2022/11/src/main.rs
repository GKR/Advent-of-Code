use std::fs;

struct Monkey {
    starting_items: Vec<i32>,
    operation: String,
    operation_value: String,
    test_divisible_by: i32,
    test_true_monkey: i32,
    test_false_monkey: i32
}

#[derive(Clone, Copy)]
struct Item {
    base_2: i32,
    base_3: i32,
    base_5: i32,
    base_7: i32,
    base_11: i32,
    base_13: i32,
    base_17: i32,
    base_19: i32,
    base_23: i32,
}

impl Item {
    fn init(val: i32) -> Item {
        return Item {
            base_2: val % 2,
            base_3: val % 3,
            base_5: val % 5,
            base_7: val % 7,
            base_11: val % 11,
            base_13: val % 13,
            base_17: val % 17,
            base_19: val % 19,
            base_23: val % 23,
        }
    }

    fn is_divisible(self, val: i32) -> bool {
        match val {
            2 => self.base_2 == 0,
            3 => self.base_3 == 0,
            5 => self.base_5 == 0,
            7 => self.base_7 == 0,
            11 => self.base_11 == 0,
            13 => self.base_13 == 0,
            17 => self.base_17 == 0,
            19 => self.base_19 == 0,
            23 => self.base_23 == 0,
            _ => false
        }
    }

    fn add(self, val: i32) -> Item {
        return Item {
            base_2: (self.base_2 + val) % 2,
            base_3: (self.base_3 + val) % 3,
            base_5: (self.base_5 + val) % 5,
            base_7: (self.base_7 + val) % 7,
            base_11: (self.base_11 + val) % 11,
            base_13: (self.base_13 + val) % 13,
            base_17: (self.base_17 + val) % 17,
            base_19: (self.base_19 + val) % 19,
            base_23: (self.base_23 + val) % 23,
        }
    }

    fn mul(self, val: i32) -> Item {
        return Item {
            base_2: (self.base_2 * val) % 2,
            base_3: (self.base_3 * val) % 3,
            base_5: (self.base_5 * val) % 5,
            base_7: (self.base_7 * val) % 7,
            base_11: (self.base_11 * val) % 11,
            base_13: (self.base_13 * val) % 13,
            base_17: (self.base_17 * val) % 17,
            base_19: (self.base_19 * val) % 19,
            base_23: (self.base_23 * val) % 23,
        }
    }

    fn sq(self) -> Item {
        return Item {
            base_2: (self.base_2 * self.base_2) % 2,
            base_3: (self.base_3 * self.base_3) % 3,
            base_5: (self.base_5 * self.base_5) % 5,
            base_7: (self.base_7 * self.base_7) % 7,
            base_11: (self.base_11 * self.base_11) % 11,
            base_13: (self.base_13 * self.base_13) % 13,
            base_17: (self.base_17 * self.base_17) % 17,
            base_19: (self.base_19 * self.base_19) % 19,
            base_23: (self.base_23 * self.base_23) % 23,
        }
    }
}

fn solve_part_1(monkeys: &Vec<Monkey>) {
    let mut monkey_states: Vec<Vec<i32>> = vec![vec![]; monkeys.len()];
    let mut monkey_passings: Vec<i32> = monkeys.iter().map(|_m| 0).collect();

    for m in 0..monkeys.len() {
        let cloned_state = &mut monkeys[m].starting_items.clone();
        monkey_states[m].append(cloned_state)
    }

    for _iteration in 0..20 {
        for monkey_index in 0..monkeys.len() {
            let monkey_items = monkey_states[monkey_index].clone();
            for monkey_item in monkey_items {
                let mut worry_level: i32 = 0;
                monkey_passings[monkey_index] = monkey_passings[monkey_index] + 1;

                if monkeys[monkey_index].operation == "*" {
                    if monkeys[monkey_index].operation_value == "old" {
                        worry_level = monkey_item * monkey_item;
                    } else {
                        worry_level = monkey_item * monkeys[monkey_index].operation_value.parse::<i32>().unwrap();
                    }
                } else if monkeys[monkey_index].operation == "+" {
                    if monkeys[monkey_index].operation_value == "old" {
                        worry_level = monkey_item + monkey_item;
                    } else {
                        worry_level = monkey_item + monkeys[monkey_index].operation_value.parse::<i32>().unwrap();
                    }
                }

                worry_level = worry_level / 3;
                if worry_level % monkeys[monkey_index].test_divisible_by == 0 {
                    let test_true_monkey = monkeys[monkey_index].test_true_monkey;
                    monkey_states[test_true_monkey as usize].push(worry_level);
                } else {
                    let test_false_monkey = monkeys[monkey_index].test_false_monkey;
                    monkey_states[test_false_monkey as usize].push(worry_level);
                }
            }
            monkey_states[monkey_index] = vec![];
        }
    }

    monkey_passings.sort();
    monkey_passings.reverse();
    println!("Part 1: {}", monkey_passings[0] as i64 * monkey_passings[1] as i64);
}

fn solve_part_2(monkeys: &Vec<Monkey>) {
    let mut monkey_states: Vec<Vec<Item>> = vec![vec![]; monkeys.len()];
    let mut monkey_passings: Vec<i32> = monkeys.iter().map(|_m| 0).collect();

    for m in 0..monkeys.len() {
        for i in 0..monkeys[m].starting_items.len() {
            monkey_states[m].push(Item::init(monkeys[m].starting_items[i]))
        }
    }

    for _iteration in 0..10000 { // part 1
        for monkey_index in 0..monkeys.len() {
            let monkey_items = monkey_states[monkey_index].clone();
            for monkey_item in monkey_items {
                
                monkey_passings[monkey_index] = monkey_passings[monkey_index] + 1;

                let mut monkey_item = monkey_item.clone();

                if monkeys[monkey_index].operation == "*" {
                    if monkeys[monkey_index].operation_value == "old" {
                        monkey_item = monkey_item.sq();
                    } else {
                        monkey_item = monkey_item.mul(monkeys[monkey_index].operation_value.parse::<i32>().unwrap());
                    }
                } else if monkeys[monkey_index].operation == "+" {
                    if monkeys[monkey_index].operation_value == "old" {
                        monkey_item = monkey_item.mul(2);
                    } else {
                        monkey_item = monkey_item.add(monkeys[monkey_index].operation_value.parse::<i32>().unwrap());
                    }
                }

                if monkey_item.is_divisible(monkeys[monkey_index].test_divisible_by) {
                    monkey_states[monkeys[monkey_index].test_true_monkey as usize].push(monkey_item);
                } else {
                    monkey_states[monkeys[monkey_index].test_false_monkey as usize].push(monkey_item);
                }
            }
            monkey_states[monkey_index] = vec![];
        }
    }

    monkey_passings.sort();
    monkey_passings.reverse();

    println!("Part 2: {}", monkey_passings[0] as i64 * monkey_passings[1] as i64);
}

fn main() {
    let file_contents = fs::read_to_string("input.txt").unwrap();
    let mut lines = Vec::new();
    for line in file_contents.lines() {
        lines.push(line.to_string());
    }

    let mut starting_items: Vec<i32> = vec![];
    let mut operation = String::new();
    let mut operation_value = String::new();
    let mut test_divisible_by = 0;
    let mut test_true_monkey = 0;
    let mut test_false_monkey = 0;
    let mut monkeys: Vec<Monkey> = vec![];

    for i in 0..lines.len() {
        let line_split: Vec<&str> = lines[i].split(':').collect();
        if line_split.len() < 2 {
            continue;
        }

        let line_value = line_split[1];

        let line_state = i % 7;
        if line_state == 1 { // Starting items
            starting_items = line_value.split(",").map(|num| num.trim().parse::<i32>().unwrap()).collect();
        } else if line_state == 2 { // Operation
            let parsed_op: Vec<&str> = line_value.split_whitespace().collect();
            operation = parsed_op[3].trim().to_string();
            operation_value = parsed_op[4].trim().to_string();
        } else if line_state == 3 { // Test
            let parsed_test: Vec<&str> = line_value.split_whitespace().collect();
            test_divisible_by = parsed_test[2].trim().to_string().parse::<i32>().unwrap();
        } else if line_state == 4 { // Test true
            let parsed_test: Vec<&str> = line_value.split_whitespace().collect();
            test_true_monkey = parsed_test[3].trim().to_string().parse::<i32>().unwrap();
        } else if line_state == 5 { // Test false
            let parsed_test: Vec<&str> = line_value.split_whitespace().collect();
            test_false_monkey = parsed_test[3].trim().to_string().parse::<i32>().unwrap();
        }

        if (line_state == 0 && i > 0) || i == lines.len() - 1 { // New monkey
            monkeys.push(Monkey {
                starting_items: starting_items.clone(),
                operation: operation.clone(),
                operation_value: operation_value.clone(),
                test_divisible_by: test_divisible_by,
                test_true_monkey: test_true_monkey,
                test_false_monkey: test_false_monkey
            });
        }
    }

    solve_part_1(&monkeys);
    solve_part_2(&monkeys);
}
