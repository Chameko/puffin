use anyhow::Result;

const VERSION: &str = env!("CARGO_PKG_VERSION");

pub fn repl() -> Result<()> {
    let mut rl = rustyline::DefaultEditor::new()?;
    println!("Puffin -- version: {}", VERSION);
    println!("Usage:\n%help - Print out help\n%exit - Exit\n%stack - Output the stack");
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                if line.chars().nth(0) == Some('%') {
                    match line.as_str() {
                        "%exit" => break,
                        "%help" => println!("Usage:\n%help - Print out help\n%exit - Exit\n%stack - Output the stack"),
                        "%stack" => (/* Dump stack */),
                        _ => println!("Unknown operation")
                    }
                } else {
                    println!("Unknown operation")
                }
            },
            Err(_) => println!("Error reading line"),
        }
    }
    Ok(())
}
