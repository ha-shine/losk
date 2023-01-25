use losk_core::Scanner;
use std::fs::File;
use std::io::Read;
use vm::{Compiler, VM};
use walkdir::WalkDir;

#[test]
fn test_programs() {
    let source_files = WalkDir::new("../tests")
        .into_iter()
        .filter_map(|entry| entry.ok())
        .filter(|entry| matches!(entry.path().extension(), Some(extension) if extension == "lox"))
        .filter_map(|entry| {
            let mut exp_filename = entry.file_name().to_os_string();
            exp_filename.push(".out");

            let parent = entry.path().parent().unwrap();
            let exp_filepath = parent.join(exp_filename);

            if exp_filepath.exists() {
                Some((entry, exp_filepath))
            } else {
                None
            }
        });

    let mut total = 0;

    for (src_path, exp_path) in source_files {
        println!("🕑 Running test: {}", src_path.path().display());

        let mut src_content = String::new();
        let mut exp_content = String::new();

        File::open(src_path.path())
            .unwrap()
            .read_to_string(&mut src_content)
            .unwrap();
        File::open(exp_path)
            .unwrap()
            .read_to_string(&mut exp_content)
            .unwrap();

        let mut scanner = Scanner::new();
        let stream = scanner.scan_tokens(&src_content);

        let compiler = Compiler::new();
        let compiled = compiler.compile(stream);

        let function = match compiled {
            Ok(fun) => fun,
            Err(errs) => {
                let err_combined: String = errs.into_iter().map(|err| err.to_string()).fold(
                    String::new(),
                    |mut acc, item| {
                        acc.push_str(&item);
                        acc.push('\n');
                        acc
                    },
                );

                assert_eq!(exp_content, err_combined);
                println!("✅ Test complete: {}", src_path.path().display());
                continue;
            }
        };

        let mut output: Vec<u8> = Vec::new();
        let mut vm = VM::new(&mut output);
        match vm.run(function) {
            Ok(_) => {
                assert_eq!(exp_content, std::str::from_utf8(&output).unwrap())
            }
            Err(err) => {
                assert_eq!(exp_content, err.to_string())
            }
        }

        println!("✅ Test complete: {}", src_path.path().display());
        total += 1;
    }

    println!("✅ Ran {} tests", total)
}
