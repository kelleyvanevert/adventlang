use compiler::JIT;
use parser::{AdventlangParser, print_parse_error};
use type_checker::{TypeCheckerCtx, print_type_error};

use crate::lower::LoweringPass;

mod lower;

pub fn main() {
    let source = "
        let a = 4
        let b = a + 2
        print(a)
        bla([4])
        bla([true])

        fn bla<T>(a: [T]) { 42 }
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

    println!("lowered:\n\n{}\n", lowered_doc);

    // let mut jit_compiler = JIT::new(&parse_result, &type_checker);

    // if let Err(err) = jit_compiler.compile_doc(&parse_result.document) {
    //     println!("===============================");
    //     println!("Could not compile the document:");
    //     println!("ERR: {:?}", err.to_string());
    //     println!("===============================");
    //     panic!();
    // }
}
