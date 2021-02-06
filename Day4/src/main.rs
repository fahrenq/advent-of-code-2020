use regex::Regex;

#[derive(Debug, Clone)]
struct Passport {
    byr: Option<u32>,
    iyr: Option<u32>,
    eyr: Option<u32>,
    hgt: Option<String>,
    hcl: Option<String>,
    ecl: Option<String>,
    pid: Option<String>,
}

fn validate(p: &Passport) -> bool {
    p.byr.is_some()
        && p.iyr.is_some()
        && p.eyr.is_some()
        && p.hgt.is_some()
        && p.hcl.is_some()
        && p.ecl.is_some()
        && p.pid.is_some()
}

fn validate_part2(p: &Passport) -> bool {
    let byr = match p.byr {
        Some(_x @ 1920..=2002) => true,
        _ => false,
    };

    let iyr = match &p.iyr {
        Some(_x @ 2010..=2020) => true,
        _ => false,
    };

    let eyr = match &p.eyr {
        Some(_x @ 2020..=2030) => true,
        _ => false,
    };

    let hgt = match &p.hgt {
        Some(x) if x.ends_with("cm") => {
            (150..=193).contains(&x.replace("cm", "").parse::<u32>().unwrap())
        }
        Some(x) if x.ends_with("in") => {
            (59..=76).contains(&x.replace("in", "").parse::<u32>().unwrap())
        }
        _ => false,
    };

    let hcl = match &p.hcl {
        Some(x) => {
            let re = Regex::new(r"^#[0-9a-fA-F]{6}$").unwrap();
            re.is_match(&x)
        }
        _ => false,
    };

    let ecl = match &p.ecl {
        Some(x) => match x.as_str() {
            "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" => true,
            _ => false,
        },
        _ => false,
    };

    let pid = match &p.pid {
        Some(x) => {
            let re = Regex::new(r"^\d{9}$").unwrap();
            re.is_match(&x)
        }
        _ => false,
    };

    byr && iyr && eyr && hgt && hcl && ecl && pid
}

fn try_find_value<T>(fields: &Vec<(&str, &str)>, key: &str) -> Option<T>
where
    T: std::str::FromStr,
    <T as std::str::FromStr>::Err: std::fmt::Debug,
{
    let r = fields
        .iter()
        .find(|(k, _)| *k == key)
        .map(|(_, value)| *value)
        .map(|x| {
            x.parse::<T>().unwrap()
            // let r = x.parse::<T>();
            // if r.is_err() {
            //     println!("Could not parse value {}", x)
            // }
            // r
        });
    r
    // match r {
    //     Some(Ok(i)) => Some(i),
    //     _ => None,
    // }
}

fn parse_passport(i: String) -> Passport {
    let re = Regex::new(r"(byr|iyr|eyr|hgt|hcl|ecl|pid):\s*(\S+)(?:\s|\n|$)").unwrap();

    let fields: Vec<(&str, &str)> = re
        .captures_iter(&i)
        .map(|x| (x.get(1).unwrap().as_str(), x.get(2).unwrap().as_str()))
        .collect();

    let byr = try_find_value(&fields, "byr");
    let iyr = try_find_value(&fields, "iyr");
    let eyr = try_find_value(&fields, "eyr");
    let hgt = try_find_value(&fields, "hgt");
    let hcl = try_find_value(&fields, "hcl");
    let ecl = try_find_value(&fields, "ecl");
    let pid = try_find_value(&fields, "pid");

    let passport = Passport {
        byr,
        iyr,
        eyr,
        hgt,
        hcl,
        ecl,
        pid,
    };

    passport
}

fn main() {
    let input = include_str!("./input.txt");
    let all_passports: Vec<Passport> = input
        .split("\n\n")
        .map(|i| i.to_string())
        .map(parse_passport)
        .collect();
    let valid_passports: Vec<Passport> = all_passports
        .iter()
        .filter(|a| validate(a))
        .cloned()
        .collect();
    let valid_passports_part2: Vec<Passport> = all_passports
        .iter()
        .filter(|a| validate_part2(a))
        .cloned()
        .collect();

    println!("All passports count: {:?}", all_passports.iter().count());
    println!(
        "Valid passports count: {:?}",
        valid_passports.iter().count()
    );
    println!(
        "Valid passports count (part 2): {:?}",
        valid_passports_part2.iter().count()
    );
}
