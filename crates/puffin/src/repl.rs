use anyhow::Result;
use puffin_codegen::Compiler;
use puffin_parser::{lexer::Lexer, parser::Parser, ast_parser::ASTParser};

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
                    let tokens = Lexer::new(&line).start_scan();
                    let src = vec![line.as_str()];
                    let parse = Parser::new(tokens, "repl", &src).parse();
                    if parse.errors.len() != 0 {
                        for error in parse.errors {
                            println!("{}", error);
                        }
                        println!("Failed to run due to errors");
                        continue;
                    } else {
                        let ast = ASTParser::new().parse_ast(&parse.green_node);
                        let comp = Compiler::new(&src, "repl");
                        match comp.generate_bytecode(ast) {
                            Ok(mut vm) => {
                                vm.run();
                            }
                            Err(comp) => {
                                match comp.check_unresolved_requests() {
                                    Err(errors) => {
                                        for err in errors {
                                            println!("{}", err);
                                        }
                                    }
                                    Ok(mut vm) => {
                                        vm.run();
                                    }
                                }
                            }
                        }
                    }
                }
            },
            Err(_) => println!("Error reading line"),
        }
    }
    Ok(())
}
