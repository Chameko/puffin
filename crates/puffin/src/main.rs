use clap::{Parser, Subcommand};
use puffin_codegen::Compiler;
use puffin_parser::{lexer::Lexer, parser, ast_parser::ASTParser};
mod repl;

/// Puffin
#[derive(Parser)]
#[command(author = "Chameko")]
#[command(about = "The puffin interpreter")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

/// Commands
#[derive(Subcommand)]
enum Command {
    /// Compile and run this file
    Run {
        /// use once for stack dump, use twice for dissasembly
        #[arg(short, long, action = clap::ArgAction::Count)]
        debug: u8,
        /// the file containing the code to be run
        file: String,
    },
    /// Enter the REPL
    REPL,
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Command::Run { file, debug } => {
            match std::fs::read_to_string(&file) {
                Ok(src) => {
                    let tokens = Lexer::new(&src).start_scan();
                    let src = src.split_inclusive('\n').collect();
                    let parse = parser::Parser::new(tokens, &file, &src).parse();
                    if parse.errors.len() == 0 {
                        let ast = ASTParser::new().parse_ast(&parse.green_node) ;
                        let comp = Compiler::new(&src, "repl");
                        match comp.generate_bytecode(ast) {
                            Ok(mut vm) => {
                                if debug == 1 {
                                    vm.run_with_stack_trace();
                                } else if debug == 2 {
                                    println!("{}", puffin_vm::disassembler::dissasemble(vm));
                                } else {
                                    vm.run();
                                }
                            }
                            Err(comp) => {
                                match comp.check_unresolved_requests() {
                                    Err(errors) => {
                                        for err in errors {
                                            println!("{}", err);
                                        }
                                    }
                                    Ok(mut vm) => {
                                        if debug == 1 {
                                            vm.run_with_stack_trace();
                                        } else if debug == 2 {
                                            println!("{}", puffin_vm::disassembler::dissasemble(vm));
                                        } else {
                                            vm.run();
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        for err in parse.errors {
                            println!("{}", err);
                        }
                    }
                }
                Err(e) => {
                    println!("Error: {}", e);
                }
            }
        }
        Command::REPL => {
            if let Err(e) = repl::repl() {
                println!("REPL failed: {}", e);
            }
        },
    }
}

