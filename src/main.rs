use std::io::{stdin, stdout, Write};

use anyhow::Context;
use interpreter::{Executor, Value};

fn main() -> anyhow::Result<()> {
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

        match executor.evaluate(&input) {
            Ok(value) => {
                if let Value::Void = value {
                    continue;
                }

                println!("{value}");
            }
            Err(err) => println!("{err:#}"),
        }
    }
}
