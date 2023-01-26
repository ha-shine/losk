# Run a specific benchmark
bench NAME:
    mkdir -p ./bench
    hyperfine --warmup 3 --parameter-list \
        --export-markdown ./bench/{{NAME}}.md \
        --export-csv ./bench/{{NAME}}.csv \
        './clox-rs ./tests/benchmark/{{NAME}}.lox' \
        './clox ./tests/benchmark/{{NAME}}.lox' \
        'python3 ./tests/benchmark/{{NAME}}.py'

# Run all benchmarks
bench-all: (bench "binary_trees") (bench "equality") (bench "fib") (bench "instantiation") (bench "invocation") (bench "method_call") (bench "properties") (bench "trees") (bench "zoo") (bench "zoo_batch")
