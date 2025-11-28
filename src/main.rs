mod diagnostics;

mod parser;
use crate::diagnostics::show_diagnostics;
use anyhow::{Result, anyhow};
use clap::Parser;
use parser::{Parser as CParser, TopLevel};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(value_name = "FILE")]
    path: String,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let path = args.path;
    let source = std::fs::read_to_string(&path).unwrap();

    let top_level = match CParser::new(&source) {
        Ok(mut parser) => match parser.parse_program() {
            Ok(program) => program,
            Err(e) => {
                show_diagnostics(&source, &path, &[e]);
                return Err(anyhow!("Parsing failed"));
            }
        },
        Err(errors) => {
            show_diagnostics(source, path.as_str(), &errors);
            return Err(anyhow!("Could not tokenize input"));
        }
    };

    for stmt in top_level {
        match *stmt {
            TopLevel::Function(ref func) => {
                println!("Parsed function: {}", *func.name);
            }
            _ => {
                println!("Parsed top-level item.");
            }
        }
    }

    Ok(())
}
