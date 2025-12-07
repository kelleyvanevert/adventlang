use compiler::{RuntimeOverrides, compile::CompileError, compile_to_fn, runtime};
use parser::print_parse_error;
use rstest::rstest;
use std::cell::RefCell;

thread_local! {
    pub static PRINTED_LINES: RefCell<Vec<String>> = RefCell::new(vec![]);
}

#[allow(unused)]
pub extern "C" fn override_al_print_int(value: u64) {
    PRINTED_LINES.with_borrow_mut(|lines| {
        lines.push(value.to_string());
    });
}

#[allow(unused)]
pub extern "C" fn override_al_print_str(ptr: *mut u64) {
    runtime::str::using_al_str(ptr, |str| {
        PRINTED_LINES.with_borrow_mut(|lines| {
            lines.push(str.to_string());
        });
    });
}

#[rstest]
fn run_corpus_test_case(
    #[files("tests/corpus/**/*.al")]
    #[mode = str]
    source: &str,
) {
    let check_output = source
        .lines()
        .filter_map(|str| str.split_once("/// ").map(|t| t.1))
        .collect::<Vec<_>>()
        .join("\n");

    let runtime_overrides = RuntimeOverrides {
        al_print_int: Some(override_al_print_int as *const u8),
        al_print_str: Some(override_al_print_str as *const u8),
    };

    let compiled_fn = match compile_to_fn(source, runtime_overrides) {
        Ok(f) => f,
        Err(CompileError::ParseError(parse_error)) => {
            println!("=============================");
            println!("Could not parse the document:");
            println!("ERR: {:?}", parse_error.to_string());
            println!("=============================");
            print_parse_error(source, parse_error);
            panic!();
        }
        Err(CompileError::TypeError(type_error)) => {
            println!("==================================");
            println!("Could not type-check the document:");
            println!("ERR: {:?}", type_error.to_string());
            println!("==================================");
            // print_type_error(&parse_result, type_error);
            panic!();
        }
        Err(err) => {
            println!("===============================");
            println!("Could not compile the document:");
            println!("ERR: {:?}", err.to_string());
            println!("===============================");
            panic!();
        }
    };

    compiled_fn();

    let actual_output = PRINTED_LINES.take().join("\n");

    assert_eq!(actual_output, check_output);
}
