use parser::Parser;
use std::io::{self, Write};

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut _stderr = io::stderr();

    let mut input = String::new();

    loop {
        println!("\n\n");
        write!(stdout, "→ ")?;
        stdout.flush()?;

        stdin.read_line(&mut input)?;

        let parse = Parser::new(&input).parse();
        println!("{}", parse.debug_tree());

        input.clear();
    }
}
