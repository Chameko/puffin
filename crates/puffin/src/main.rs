use clap::{Parser, Subcommand};
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
}

