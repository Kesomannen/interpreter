use std::{
    env::current_dir,
    fs,
    io::{stdin, stdout, Write},
    path::{Path, PathBuf},
};

use anyhow::Context;
use clap::Parser;
use interpreter::Executor;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    file: Option<PathBuf>,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli.file {
        Some(path) => run_file(&path),
        None => run_interactive(),
    }
}

fn run_file(path: &Path) -> anyhow::Result<()> {
    let path = current_dir()
        .context("failed to determine current directory")?
        .join(path);

    let str = fs::read_to_string(path).context("failed to read file")?;
    Executor::new().exec(&str)?;

    Ok(())
}

fn run_interactive() -> anyhow::Result<()> {
    let mut executor = Executor::new();

    loop {
        print!(">> ");
        stdout().flush().ok();

        let mut input = String::new();
        stdin()
            .read_line(&mut input)
            .context("failed to read input string")?;

        input = input.trim().to_owned();

        if input == "exit" {
            return Ok(());
        }

        match executor.eval(&input) {
            Ok(value) => println!("{value}"),
            Err(err) => println!("{err:#}"),
        }
    }
}
