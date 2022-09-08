#![feature(path_file_prefix)]
#![feature(trait_alias)]
#![feature(absolute_path)]
mod asm;
mod common;
mod jack;
mod parse;
mod symbol;
mod vm;

use asm::HackAssembler;
use jack::JackAnalyzer;
use vm::VMTranslator;

#[derive(PartialEq, Eq)]
enum Command {
    HackAssembler,
    VMTranslator,
    JackAnalyzer,
    Unknown,
}

impl From<&str> for Command {
    fn from(item: &str) -> Self {
        match item {
            "HackAssembler" => Self::HackAssembler,
            "VMTranslator" => Self::VMTranslator,
            "JackAnalyzer" => Self::JackAnalyzer,
            _ => Self::Unknown,
        }
    }
}

impl Command {
    fn name(&self) -> &'static str {
        match self {
            &Self::HackAssembler => "HackAssembler",
            &Self::VMTranslator => "VMTranslator",
            &Self::JackAnalyzer => "JackAnalyzer",
            &Self::Unknown => "Unknown",
        }
    }
}

fn main() {
    let command: Command = std::env::args()
        .nth(1)
        .map(|command| command.as_str().into())
        .filter(|cmd| cmd != &Command::Unknown)
        .expect("the first argument must be a supported command");
    let cmd_args: Vec<String> = std::env::args().skip(2).collect();
    match command {
        Command::HackAssembler => {
            HackAssembler::do_main(cmd_args.iter().map(|arg| arg.as_str()).collect())
        }
        Command::VMTranslator => {
            VMTranslator::do_main(cmd_args.iter().map(|arg| arg.as_str()).collect())
                .expect("failed to run");
        }
        Command::JackAnalyzer => {
            JackAnalyzer::do_main(cmd_args.iter().map(|arg| arg.as_str()).collect())
                .expect("failed to run");
        }
        _ => panic!("unsupported command: {}", command.name()),
    }
}
