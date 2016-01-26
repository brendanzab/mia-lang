extern crate linenoise;
extern crate mia;

use mia::Words;

const HI_THERE: &'static str = r#"
|\/|. _
|  ||(_|  - a concatenative language

"#;

const BYE_BYE: &'static str = r#"
(^._.^)ﾉ
"#;

fn main() {
    let words = Words::std();

    println!("{}", HI_THERE);

    linenoise::history_load("history");

    while let Some(input) = linenoise::input("mia> ") {
        linenoise::history_add(&input);

        match input.parse() {
            Ok(stack) => {
                match mia::eval(stack, &words) {
                    Ok(x) => println!("{}", x),
                    Err(x) => println!("error: {}", x),
                }
            },
            Err(msg) => println!("parse error: {}", msg),
        };
    }

    linenoise::history_save("history");

    println!("{}", BYE_BYE);
}
