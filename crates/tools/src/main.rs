use clap::{Parser, Subcommand};

#[derive(Parser)]
#[clap(name = "tasks", version, author)]
struct Args {
    #[clap(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Generate rust syntax files
    GenSyntax
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();
    match args.command {
        Commands::GenSyntax => tools::generate(tools::Mode::Update)?,
    }
    Ok(())
}