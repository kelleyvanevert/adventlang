use compiler::{JIT, lower::LoweringPass};
use parser::{AdventlangParser, print_parse_error};
use type_checker::{TypeCheckerCtx, print_type_error};

pub fn main() {
    //     let source = "
    //         let a = 4
    //         let b = a + 2
    //         print(b)
    //         print(stdin() :trim)
    //         print(b)
    //         print([1, 2, 3, 4] :len)

    // //        bla([true])
    // //        bla([4], a)
    // //
    // //        let bla = [1, 2, 3]:len
    // //
    // //        fn bla<T>(arr: [T], index: int) -> T {
    // //            arr[index]
    // //        }
    // //
    // //        fn bla<T>(arr: [T]) -> T {
    // //            arr[0]
    // //        }
    //     ";

    let source = "
        fn add_one(n: int) {
          n + 1
        }

        fn add_two(n: int) {
          n + 2
        }

        fn some_fn(f: fn(int) -> int) -> int {
          f(10)
        }

        print(some_fn(add_one)) // 11
        print(some_fn(add_two)) // 12
    ";

    let mut parser = AdventlangParser::new();
    let parse_result = match parser.parse_document(source) {
        Ok(r) => r,
        Err(err) => {
            println!("=============================");
            println!("Could not parse the document:");
            println!("ERR: {:?}", err.to_string());
            println!("=============================");
            print_parse_error(source, err);
            panic!();
        }
    };

    let mut type_checker = TypeCheckerCtx::new();
    if let Err(err) = type_checker.typecheck(&parse_result.document) {
        println!("==================================");
        println!("Could not type-check the document:");
        println!("ERR: {:?}", err.to_string());
        println!("==================================");
        print_type_error(&parse_result, err);
        panic!();
    }

    let mut lowering_pass = LoweringPass::new(&type_checker);

    let lowered_doc = lowering_pass.lower_doc(&parse_result.document);

    println!("\nLOWERED:\n\n{}\n\n======\n", lowered_doc);

    let mut jit_compiler = JIT::new(&type_checker);

    match jit_compiler.compile_doc(&lowered_doc) {
        Err(err) => {
            println!("===============================");
            println!("Could not compile the document:");
            println!("ERR: {}", err.to_string());
            println!("===============================");
            panic!();
        }
        Ok(code_ptr) => {
            let code_fn = unsafe { std::mem::transmute::<_, fn() -> ()>(code_ptr) };
            println!("code ptr {:?}", code_ptr);
            code_fn();
        }
    }
}
