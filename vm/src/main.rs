use std::{env, io};
use std::fs::File;
use std::io::{Read, stdout};
use std::process::exit;

use losk_core::Scanner;
use vm::{Compiler, VM};

fn read_file(name: &str) -> Result<String, io::Error> {
    let mut src = String::new();
    let mut file = File::open(name)?;
    file.read_to_string(&mut src)?;
    Ok(src)
}

fn main() {
    let mut args = env::args();
    let prog = args.next().unwrap();
    let name = match args.next() {
        Some(name) => name,
        None => {
            eprintln!("Usage: {} <filename>", prog);
            exit(1);
        }
    };

    let src = match read_file(&name) {
        Ok(val) => val,
        Err(err) => {
            eprintln!("Failed to read file '{}': {}.", name, err);
            exit(1);
        }
    };

    let mut scanner = Scanner::new();
    let tokens = scanner.scan_tokens(&src);
    let compiler = Compiler::new();
    let fun = match compiler.compile(tokens) {
        Ok(fun) => fun,
        Err(errs) => {
            errs.iter().for_each(|err| eprintln!("{}", err));
            exit(1);
        }
    };

    let out = &mut stdout();
    let mut vm = VM::new(out);
    if let Err(err) = vm.run(fun) {
        eprintln!("{}", err);
        exit(1);
    }
}
