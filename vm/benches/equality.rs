use std::io;

use criterion::{criterion_group, criterion_main, Criterion};

use losk_core::Scanner;
use vm::{Compiler, VM};

fn benchmark(c: &mut Criterion) {
    let src = include_str!("../../tests/benchmark/equality.lox");
    let mut scanner = Scanner::new();
    let compiler = Compiler::new();
    let stream = scanner.scan_tokens(src);
    let compiled = compiler.compile(stream).unwrap();
    let mut sink = io::sink();

    c.bench_function("equality", |b| {
        b.iter(|| {
            let mut vm = VM::new(&mut sink);
            vm.run(compiled.clone()).unwrap();
        })
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default().sample_size(10);
    targets = benchmark
}
criterion_main!(benches);
