#[cfg(test)]
mod tests {
    use std::time::Instant;

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

        let doc1 = parse_document(&content).expect("Can parse with parser combinators");

        let mut doc2 = parse_document_ts(&content).expect("Can parse with Tree-sitter");
        doc2.strip_ids();

        // if doc1 != doc2 {
        //     fs::write(&format!("doc1_{name}.txt"), format!("{:#?}", doc1)).unwrap();
        //     fs::write(&format!("doc2_{name}.txt"), format!("{:#?}", doc2)).unwrap();
        // }

        assert_eq!(doc1, doc2);
    }

    #[test]
    #[ignore]
    fn bench() {
        let tests = [
            "2023_day01",
            "2023_day02",
            "2023_day03",
            "2023_day04",
            "2023_day05",
            "2023_day06",
            "2023_day07",
            "2023_day08",
            "2023_day09",
            "2023_day10",
            "2023_day11",
            "2023_day12",
            "2023_day13",
            "2023_day14",
            "2024_day01",
            "2024_day02",
            "2024_day03",
            "2024_day04",
            "2024_day05",
            "2024_day06",
            "2024_day07",
            "2024_day08",
        ];

        let contents = tests.map(|name| {
            std::fs::read_to_string(format!("./aoc/{name}.al")).expect("can read aoc file")
        });

        let using_parser_combinators = {
            let t0 = Instant::now();
            for source in &contents {
                parse_document(source).expect("can parse");
            }
            t0.elapsed()
        };

        let using_tree_sitter = {
            let t0 = Instant::now();
            for source in &contents {
                parse_document_ts(source).expect("can parse");
            }
            t0.elapsed()
        };

        panic!(
            "Total parsing time:\n- using parser combinators: {:?}\n- using tree-sitter: {:?}",
            using_parser_combinators, using_tree_sitter
        );
        // Total parsing time:
        // - using parser combinators: 1.315s
        // - using tree-sitter: 8.55ms
    }
}
