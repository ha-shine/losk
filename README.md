# Losk

Losk is the educational lox interpreter (and VM) implementation from the book 
[Crafting Compilers](https://craftinginterpreters.com/). The VM is pretty complete at this moment as it has been
given enough care to pass all the tests from the original repo. 
The interpreter might have some issues, and a major one being that it's possible to create cycle between objects 
as `Rc` is used liberally to represent pointers. I have no intention to refine the interpreter and only interested in
the VM.

## Repository Layout

- `assets/` - All the static files used in the project, mainly just benchmark photos.
- `interpreter/` - Interpreter crate that should resemble `jlox` from the book.
- `losk-core/` - The core crate that is used by both the interpreter and the VM. As a side not, I named it `core` at the 
   beginning but found that the name interferes with Rust in weird ways.
- `scripts/` - This is where I store my files for generating plots.
- `tests/` - The test suite from the [source repository](https://github.com/munificent/craftinginterpreters), with `.out`
  expected outputs in `.out`.
- `vm/` - VM crate aka Rust version of `clox`

Both the `interpreter/` and `vm/` folders contain `TODOS.md` files where I dumped the challenges I'm interested in exploring
from the textbooks, plus a few todos that I think would improve the code in someway. But as you already know, the TODOs
are unlikely to be completed and that's why they are called TODOs, heh.

## Benchmarks

If you want to compare the performance between versions, I have translated the benchmark programs to python and 
documented them in [BENCHMARK.md](./BENCHMARK.md). You can find every optimization I have done in that file as well.
The file also include benchmarks of the unsafe but more performant version which is much closer to clox.