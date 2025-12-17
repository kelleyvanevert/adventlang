use crate::{Env, TypeCheckerCtx, TypeErrorKind, print_type_error};
use parser::{AdventlangParser, ast::SExpPrintJob};

fn run_test_case(
    test_file_name: &str,
    lineno: usize,
    description: &str,
    expectation: &str,
    debug: bool,
    _error_location: Option<(usize, usize)>,
    source: &str,
) {
    let Ok(parse_result) = AdventlangParser::new().parse_document(&source) else {
        panic!("Can't parse test case source, file: `{test_file_name}`, line {lineno}");
    };

    let mut ctx = TypeCheckerCtx::new();
    match ctx.typecheck(&parse_result.document) {
        Ok(_typed_doc) => {
            if debug {
                println!(
                    "{}",
                    SExpPrintJob {
                        document: &parse_result.document,
                        annotations: ctx
                            .types
                            .iter()
                            .map(|(id, ty)| { (*id, (id, ty)) })
                            .collect(),
                    }
                );
                ctx.debug_info();
            }

            if expectation != "ok" {
                println!("");
                println!("====================");
                println!("Document should not type-check, but did");
                println!("- test case: `{test_file_name}`, line {lineno}");
                println!("- description: {description}");
                println!("- expectation: {expectation}");
                println!("====================");
                println!("{source}");
                println!(
                    "{}",
                    SExpPrintJob {
                        document: &parse_result.document,
                        annotations: ctx.types.clone(),
                    }
                );
                panic!();
            }

            if debug {
                panic!("Nothing wrong, just debugging");
            }
        }
        Err(err) => {
            if debug {
                println!(
                    "{}",
                    SExpPrintJob {
                        document: &parse_result.document,
                        annotations: ctx.types.clone(),
                    }
                );
                ctx.debug_info();
            }

            if expectation == "ok" {
                println!("");
                println!("====================");
                println!("Document should type-check, but didn't");
                println!("- test case: `{test_file_name}`, line {lineno}");
                println!("- description: {description}");
                println!("====================");
                println!(
                    "{}",
                    SExpPrintJob {
                        document: &parse_result.document,
                        annotations: ctx.types.clone(),
                    }
                );
                ctx.debug_disjoint_sets();
                print_type_error(&parse_result, err);
                panic!();
            }

            if expectation == "err" {
                // no expectations
                return;
            }

            let mut parser = AdventlangParser::new();

            let expected_err = expectation.trim_start_matches("err:").trim();

            match (expected_err, err.kind.clone()) {
                ("DependencyCycle", TypeErrorKind::DependencyCycle { .. }) => {}
                ("NoOverload", TypeErrorKind::NoOverload) => {}
                ("ArgsMismatch", TypeErrorKind::ArgsMismatch(_, _)) => {}
                ("InfiniteType", TypeErrorKind::InfiniteType(_, _)) => {}
                ("NotCallable", TypeErrorKind::NotCallable(_)) => {}
                ("GenericsMismatch", TypeErrorKind::GenericsMismatch) => {}
                ("UnknownLocal", TypeErrorKind::UnknownLocal(_)) => {}
                (diff, TypeErrorKind::NotEqual(le, ri)) if diff.contains(" != ") => {
                    let (a, b) = diff.split_once(" != ").unwrap();
                    let a = ctx
                        .convert_hint_to_type(&Env::new(), &parser.parse_type(a).unwrap())
                        .unwrap();
                    let b = ctx
                        .convert_hint_to_type(&Env::new(), &parser.parse_type(b).unwrap())
                        .unwrap();

                    let a = format!("{a}");
                    let b = format!("{b}");
                    let le = format!("{le}");
                    let ri = format!("{ri}");

                    if a == le && b == ri || a == ri && b == le {
                        // all good
                    } else {
                        println!("====================");
                        println!(
                            "Document correctly failed at type-check, but with unexpected error"
                        );
                        println!("- test case: `{test_file_name}`, line {lineno}");
                        println!("- description: {description}");
                        println!("- expected: {} != {}", a, b);
                        println!("- encountered: {} != {}", le, ri);
                        println!("====================");
                        print_type_error(&parse_result, err);
                        panic!();
                    }
                }
                (_, kind) => {
                    println!("====================");
                    println!("Document correctly failed at type-check, but with unexpected error");
                    println!("- test case: `{test_file_name}`, line {lineno}");
                    println!("- description: {description}");
                    println!("- expected: {expected_err}");
                    println!("- encountered: {kind:?}");
                    println!("====================");
                    print_type_error(&parse_result, err);
                    panic!()
                }
            }

            if debug {
                panic!("Nothing wrong, just debugging");
            }
        }
    }
}

macro_rules! run_test_cases_in_file {
    ($filename:ident) => {
        #[test]
        fn $filename() {
            let filename = concat!(stringify!($filename), ".al");
            let contents = include_str!(concat!("../tests/", stringify!($filename), ".al"));
            let lines = contents.lines();
            let mut test_cases = vec![];

            let mut status = "test";
            let mut lineno = 0;
            let mut description: Vec<&str> = vec![];
            let mut expectation = "";
            let mut skip = false;
            let mut debug = false;
            let mut only = false;
            let mut error_location = None;
            let mut test_lines: Vec<&str> = vec![];

            'gather: for (i, line) in lines.enumerate() {
                if line.starts_with("// skip-all") {
                    return;
                } else if status == "test" && line.starts_with("// ======") {
                    if expectation.len() > 0 {
                        let test_case = (
                            description.join("\n"),
                            lineno,
                            expectation,
                            skip,
                            debug,
                            error_location.clone(),
                            test_lines.join("\n"),
                        );
                        if only {
                            test_cases = vec![test_case];
                        } else {
                            test_cases.push(test_case);
                        }
                    }
                    status = "meta";
                    lineno = i;
                    description = vec![];
                    expectation = "";
                    skip = false;
                    debug = false;
                    error_location = None;
                    test_lines = vec![];
                    if only {
                        break 'gather;
                    }
                } else if status == "meta" && line.starts_with("// skip") {
                    skip = true;
                } else if status == "meta" && line.starts_with("// ok") {
                    expectation = "ok";
                } else if status == "meta" && line.starts_with("// only") {
                    only = true;
                } else if status == "meta" && line.starts_with("// debug") {
                    debug = true;
                } else if status == "meta" && line.starts_with("// err") {
                    expectation = &line[3..].trim();
                } else if status == "meta" && line.starts_with("// ======") {
                    if expectation.len() == 0 {
                        panic!("No expectation for test, file `{filename}`, line {i}");
                    }
                    status = "test";
                } else if status == "meta" && line.starts_with("//") {
                    description.push(&line[3.min(line.len())..]);
                } else if status == "meta" {
                    panic!("Can't parse line in meta block, file `{filename}`, line {i}");
                } else if status == "test"
                    && let Some((before, _after)) = line.split_once("^here")
                {
                    error_location = Some((test_lines.len(), before.len()));
                } else if status == "test" {
                    test_lines.push(line);
                } else {
                    panic!("Can't parse line in test, file `{filename}`, line {i}");
                }
            }

            if expectation.len() > 0 {
                let test_case = (
                    description.join("\n"),
                    lineno,
                    expectation,
                    skip,
                    debug,
                    error_location.clone(),
                    test_lines.join("\n"),
                );
                if only {
                    test_cases = vec![test_case];
                } else {
                    test_cases.push(test_case);
                }
            }

            for (description, lineno, expectation, skip, debug, error_location, source) in
                test_cases
            {
                if !skip {
                    run_test_case(
                        filename,
                        lineno,
                        &description,
                        expectation,
                        debug,
                        error_location,
                        &source,
                    );
                }
            }
        }
    };
}

run_test_cases_in_file!(misc);
run_test_cases_in_file!(looping_and_breaking);
run_test_cases_in_file!(if_branches);
run_test_cases_in_file!(declarations_and_assignments);
run_test_cases_in_file!(operator_overloading);
run_test_cases_in_file!(generics);
run_test_cases_in_file!(function_calls);
run_test_cases_in_file!(nullability);
run_test_cases_in_file!(named_fn_overloading);
run_test_cases_in_file!(lists_dicts_tuples_indexing);
run_test_cases_in_file!(structs);
run_test_cases_in_file!(fn_types);
run_test_cases_in_file!(aoc_examples);
