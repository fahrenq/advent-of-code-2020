use itertools::Itertools;

const ROW_UB: u32 = 127;
const COL_UB: u32 = 7;

enum Token {
    UpperHalf,
    LowerHalf,
}

fn get_token(i: &char) -> Token {
    match *i {
        'F' | 'L' => Token::LowerHalf,
        'B' | 'R' => Token::UpperHalf,
        x => panic!("unknown char {}", x),
    }
}

fn calculate(ub: u32, token: &str) -> u32 {
    let init: Vec<u32> = (0..=ub).collect();
    token.chars().fold(init, |acc, i| match get_token(&i) {
        Token::UpperHalf => acc[acc.iter().count() / 2..].to_vec(),
        Token::LowerHalf => acc[..acc.iter().count() / 2].to_vec(),
    })[0]
}

fn row_substring(s: &str) -> &str {
    &s[..7]
}

fn col_substring(s: &str) -> &str {
    &s[7..]
}

fn main() {
    let seat_ids: Vec<u32> = include_str!("./input.txt")
        .lines()
        .map(|i| {
            let row = calculate(ROW_UB, row_substring(&i));
            let col = calculate(COL_UB, col_substring(&i));
            (row, col)
        })
        .map(|(row, col)| row * 8 + col)
        .collect();

    let seat_ids_sorted: Vec<u32> = seat_ids.into_iter().sorted().collect();
    let highest_seat_id = seat_ids_sorted.iter().max().unwrap();
    let my_seat_id: u32 = seat_ids_sorted
        .iter()
        .tuple_windows::<(&u32, &u32)>()
        .filter(|(a, b)| **a + 2 == **b)
        .map(|(a, _)| a + 1)
        .next()
        .unwrap();

    println!("Highest seat id: {}", highest_seat_id);
    println!("My seat id: {}", my_seat_id);
}
