use itertools::Itertools;

fn main() {
    let input = include_str!("./input.txt");

    let result_p1: usize = input
        .split("\n\n")
        .map(|group| {
            let per_group_all = group
                .lines()
                .map(|p| p.as_bytes().to_vec())
                .collect::<Vec<Vec<u8>>>()
                .concat();
            // println!("per group all: {:?}", per_group_all)
            let per_group = per_group_all.into_iter().unique().collect::<Vec<u8>>();
            // println!("per group: {:?}", per_group);
            per_group.iter().count()
        })
        .sum::<usize>();
    println!("Result for part 1: {}", result_p1);

    let result_p2: usize = input
        .split("\n\n")
        .map(|group| {
            let per_person = group
                .lines()
                .map(|p| p.as_bytes().to_vec())
                .collect::<Vec<Vec<u8>>>();
            // println!("per person: {:?}", per_person);
            let per_group = per_person[0]
                .iter()
                .filter(|k| per_person.iter().all(|s| s.contains(k)))
                .collect::<Vec<&u8>>();
            // println!("per group: {:?}", per_group);
            per_group.iter().count()
        })
        .sum::<usize>();
    println!("Result for part 2: {}", result_p2);
}
