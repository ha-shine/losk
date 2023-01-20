use criterion::{criterion_group, criterion_main, Criterion};
use losk_core::Scanner;
use std::io;
use vm::{Compiler, VM};

fn benchmark(c: &mut Criterion) {
    let src = include_str!("../../data/fib.lox");
    let mut scanner = Scanner::new();
    let compiler = Compiler::new();
    let stream = scanner.scan_tokens(src);
    let compiled = compiler.compile(stream).unwrap();
    let mut sink = io::sink();

    c.bench_function("fib 20", |b| {
        b.iter(|| {
            let mut vm = VM::new(&mut sink);
            vm.run(compiled.clone()).unwrap();
        })
    });
}

criterion_group!(benches, benchmark);
criterion_main!(benches);
