#[cfg(test)]
mod tests {
    use std::fs;

    use pretty_assertions::assert_eq;
    use test_case::test_case;

    use parser::{parse_document, parse_document_ts};

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
    #[test_case("2024_day01")]
    #[test_case("2024_day02")]
    #[test_case("2024_day03")]
    #[test_case("2024_day04")]
    #[test_case("2024_day05")]
    #[test_case("2024_day06")]
    #[test_case("2024_day07")]
    #[test_case("2024_day08")]
    fn test_parse_aoc(name: &str) {
        let content =
            std::fs::read_to_string(format!("./aoc/{name}.al")).expect("can read aoc file");

        let doc1 = parse_document(&content);
        assert!(doc1.is_some());

        let doc2 = parse_document_ts(&content);
        assert!(doc2.is_some());

        if doc1 != doc2 {
            fs::write(&format!("doc1_{name}.txt"), format!("{:#?}", doc1)).unwrap();
            fs::write(&format!("doc2_{name}.txt"), format!("{:#?}", doc2)).unwrap();
        }

        assert_eq!(doc1, doc2);
    }
}
