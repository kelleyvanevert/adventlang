use compiler::{RuntimeOverrides, compile_to_fn};

pub fn main() {
    let f = compile_to_fn(
        "
            let list = [1,2,3]
            list :push 4
            print(list[0])
        ",
        RuntimeOverrides::none(),
    )
    .unwrap();

    f();
}
