#[cfg(test)]
mod tests {
    use test_case::test_case;

    use parser::parse_document;

    #[test_case("2023_day01")]
    #[test_case("2023_day02")]
    #[test_case("2023_day03")]
    #[test_case("2023_day04")]
    #[test_case("2023_day05")]
    #[test_case("2023_day06")]
    #[test_case("2023_day07")]
    #[test_case("2023_day08")]
    #[test_case("2023_day09")]
    #[test_case("2023_day10")]
    #[test_case("2023_day11")]
    #[test_case("2023_day12")]
    #[test_case("2023_day13")]
    #[test_case("2023_day14")]
    fn test_parse_aoc_2023_day01(name: &str) {
        let content =
            std::fs::read_to_string(format!("./aoc/{name}.al")).expect("can read aoc file");

        assert!(parse_document(&content).is_some());
    }
}
