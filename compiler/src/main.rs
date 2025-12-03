use compiler::{JIT, lower::LoweringPass};
use parser::{AdventlangParser, print_parse_error};
use type_checker::{TypeCheckerCtx, print_type_error};

pub fn main() {
    let source = "
        let a = 4
        let b = a + 2
        print(a)
        bla([true])
        bla([4], a)

        let bla = [1, 2, 3]:len

        fn bla<T>(arr: [T], index: int) -> T {
            arr[index]
        }

        fn bla<T>(arr: [T]) -> T {
            arr[0]
        }
    ";

    // let source = "
    //     let a = 4
    //     let b = a + 6
    //     print(b)
    // ";

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

    if let Err(err) = jit_compiler.compile_doc(&lowered_doc) {
        println!("===============================");
        println!("Could not compile the document:");
        println!("ERR: {}", err.to_string());
        println!("===============================");
        panic!();
    }
}
