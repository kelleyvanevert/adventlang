#[cfg(test)]
mod tests {
    use std::time::Instant;

    use test_case::test_case;

    use parser::AdventlangParser;

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
            std::fs::read_to_string(format!("./tests/aoc/{name}.al")).expect("can read aoc file");

        AdventlangParser::new()
            .parse_document(&content)
            .expect("Can parse with Tree-sitter");
    }

    #[test]
    fn bench_type_parsing() {
        let stdlib = "
            // MISC.
            // ======

            print: fn(str) -> nil
            assert: fn(bool) -> nil


            // STRINGS
            // ======

            trim: fn(str) -> str
            split: fn(str, str) -> [str]
            split: fn(str, regex) -> [str]
            replace: fn(str, (str, str)) -> str
            replace: fn(str, (regex, str)) -> str
            lines: fn(str) -> [str]
            chars: fn(str) -> [str]
            starts_with: fn(str, str) -> bool

            slice: fn(str, int) -> str
            slice: fn(str, (int, int)) -> str
            slice: fn<A, B>([A], B) -> [A]

            match: fn(str, regex) -> ?[str]
            matches: fn(str, regex) -> bool
            match_all: fn(str, regex) -> [[str]]          // ... was previously heterogeneous/existential `[(str...)]`


            // CONVERSIONS
            // ======

            int: fn(str) -> int
            bool: fn<A>(A) -> bool
            float: fn<A>(A) -> float
            clone: fn<A>(A) -> A

            map: fn<K, V>([(K, V)]) -> map[K, V]
            set: fn<K>([K]) -> set[K]


            // LISTS
            // ======

            in: fn<A>(A, [A]) -> bool
            filter: fn<A, B>([A], fn(A) -> bool) -> [A]
            map: fn<A, B>([A], fn(A) -> B) -> [B]
            flat_map: fn<A, B>([A], fn(A) -> [B]) -> [B]
            filter_map: fn<A, B>([A], fn(A) -> B) -> [B]
            find_map: fn<A, B>([A], fn(A) -> B) -> ?B
            find: fn<A>([A], fn(A) -> bool) -> ?A
            any: fn<A>([A], fn(A) -> bool) -> bool
            all: fn<A>([A], fn(A) -> bool) -> bool
            all: fn([bool]) -> bool
            fold: fn<T, R>([T], R, fn(R, T) -> R) -> R
            enumerate: fn<T>([T]) -> [(int, T)]
            sort_by_key: fn<T, K>([T], fn(T) -> K) -> [T]
            chunks: fn<T>([T], int) -> [[T]]
            zip: fn<A, B>([A], [B]) -> [(A, B)]
            reverse: fn<T>([T]) -> [T]


            // NUMERICS
            // ======

            sum: fn([int]) -> int
            range: fn(int, int) -> [int]

            len: fn(str) -> int
            len: fn<A>([A]) -> int

            max: fn(int, int) -> int

            min: fn(int, int) -> int
            min: fn(float, int) -> float
            min: fn(int, float) -> float
            min: fn(float, float) -> float
            min: fn([int]) -> int
            min: fn([float]) -> float

            abs: fn(int) -> int
            abs: fn(float) -> float

            ceil: fn(float) -> int
            floor: fn(float) -> int
            round: fn(float) -> int
            round: fn(int) -> int

            sqrt: fn(int) -> float
            sqrt: fn(float) -> float

            fst: fn<A, B>((A, B)) -> A                      // TODO remove after adding tuple indexing
            snd: fn<A, B>((A, B)) -> B                      // TODO remove after adding tuple indexing

            is_some: fn<A>(?A) -> bool
            unwrap: fn<A>(?A) -> A


            // CONSTANTS
            // ======

            MAX_INT: int
            PI: float
            TAU: float


            // OPERATORS
            // ======

            ==: fn<T>(T, T) -> bool
            !=: fn<T>(T, T) -> bool
            &&: fn(bool, bool) -> bool
            ||: fn(bool, bool) -> bool

            >: fn(int, int) -> bool
            >: fn(float, float) -> bool
            <: fn(int, int) -> bool
            <: fn(float, float) -> bool
            <=: fn(int, int) -> bool
            <=: fn(float, float) -> bool
            >=: fn(int, int) -> bool
            >=: fn(float, float) -> bool

            -: fn(int, int) -> int
            -: fn(int, float) -> float
            -: fn(float, int) -> float
            -: fn(float, float) -> float

            -: fn(int) -> int
            -: fn(float) -> float

            !: fn(bool) -> bool

            +: fn(int, int) -> int
            +: fn(int, float) -> float
            +: fn(float, int) -> float
            +: fn(float, float) -> float
            +: fn(str, str) -> str

            ^: fn(int, int) -> int
            ^: fn(int, float) -> float
            ^: fn(float, int) -> float
            ^: fn(float, float) -> float

            *: fn(int, int) -> int
            *: fn(int, float) -> float
            *: fn(float, int) -> float
            *: fn(float, float) -> float

            /: fn(int, int) -> int
            /: fn(int, float) -> float
            /: fn(float, int) -> float
            /: fn(float, float) -> float

            %: fn(int, int) -> int
            %: fn(int, float) -> float
            %: fn(float, int) -> float
            %: fn(float, float) -> float

            <<: fn(int, int) -> int
        ";

        let mut parser = AdventlangParser::new();

        let types = stdlib
            .trim()
            .lines()
            .filter(|line| line.trim().len() > 0 && !line.trim().starts_with("//"))
            .map(|line| {
                let (_name, hint) = line.trim().split_once(": ").unwrap();
                let hint = hint.split_once("//").map(|t| t.0.trim()).unwrap_or(hint);
                hint
            })
            .collect::<Vec<_>>();

        let n = 50;
        let t0 = Instant::now();
        for _ in 0..n {
            for t in &types {
                parser.parse_type(t).expect("can parse");
            }
        }

        let _dt = t0.elapsed() / n;

        // panic!("Total parsing time: {:?}", dt);

        // Total parsing time:
        // - using parser combinators: 31.210999ms
        // - using tree-sitter: 784.678µs
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

        let mut ts_parser = AdventlangParser::new();

        let contents = tests.map(|name| {
            std::fs::read_to_string(format!("./tests/aoc/{name}.al")).expect("can read aoc file")
        });

        let n = 50;
        let t0 = Instant::now();
        for _ in 0..n {
            for source in &contents {
                ts_parser.parse_document(source).expect("can parse");
            }
        }

        let dt = t0.elapsed() / n;

        panic!("Total parsing time: {:?}", dt);

        // Total parsing time:
        // - using parser combinators: 1.315s
        // - using tree-sitter: 8.55ms
    }
}
