## Todos

- [ ] Generate StmtVisitor and ExprVisitor traits using procedural macro. I am not yet familiar enough with 
      proc-macros to do this task efficiently.
- [ ] Currently, all the enums (Stmt, Expr, Literal, etc.) contains fields which is not very ergonomics. Convert their
      inner fields into their own struct.
- [ ] Main CLI for interpreter that can run as REPL environment or with a source file

## Challenges

- [ ] Add support for C-style ternary operator (?:), one-liner if clauses
- [ ] `--verbose` parameter in CLI prompt for extra statistics (like execution time, etc.)
- [ ] Handle divide by 0 error
- [ ] `+` operator currently only concatenates or add numbers. Extend it to allow concatenating a number to a string
- [ ] REPL no longer supports entering single expression, it can be convenient for the user to be able to type expressions
      and see the results
- [ ] Add `break` and `continue` statement inside loops
- [ ] Add support for **anonymous functions** or **lambdas**
- [ ] Extend resolver to report an error if a local variable is never used
- [ ] Associate a unique index for each local variable declared in a scope and store that together with the depth.
      When resolving from interpreter, use that to quickly access the variable. This will be faster than using names.
- [ ] Rethink about using unique_ptr for statements and expressions
- [ ] Replace shared_ptr with local_shared_ptr from boost as thread safety is not needed
- [ ] Add `static` methods to classes, you can use metaclasses for implementation (i.e make class extends instance)