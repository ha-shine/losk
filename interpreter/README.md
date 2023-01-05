# Losk

Losk is the educational lox interpreter (and VM) implementation from the book
[Crafting Compilers](https://craftinginterpreters.com/).

# Grammar

## Precedence and associativity

Lowest to highest

| Name       | Operator          | Associates |
|------------|-------------------|------------|
| Equality   | `==` `!=`         | Left       |
| Comparison | `>` `>=` `<` `<=` | Left       |
| Term       | `-` `+`           | Left       |
| Factor     | `/` `*`           | Left       |
| Unary      | `!` `-`           | Right      |

## Parser rule table

Each rule matches expression at its precedence level or higher.
The rules are made intentionally to be right-recursive.
Terminals are in capital letters.

```

- program           -> declaration* EOF_ ;
- declaration       -> class_decl
                     | fun_decl 
                     | var_decl
                     | statement ;
- class_decl        -> "class" IDENTIFIER ( "<" IDENTIFIER )? "{" function* "}" ;
- fun_decl          -> "fun" function ;
- function          -> IDENTIFIER "(" parameters? ")" block ;
- parameters        -> IDENTIFIER ( "," IDENTIFIER )* ;
- var_decl          -> "var" IDENTIFIER ( "=" expression )? ";" ;
- statement         -> expr_statement
                     | for_statement
                     | if_statement
                     | print_statement
                     | return_statement
                     | while_statement 
                     | block ;
- for_statement     -> "for" "(" ( var_decl | expr_statement | ";" )
                       expression? ";"
                       expression? ")" statement ;
- if_statement      -> "if" "(" expression ")" statement
                       ( "else" statement )? ;
- block             -> "{" declaration* "}" ;
- expr_statement    -> expression ";" ;
- print_statement   -> "print" expression ";" ;
- return_statement  -> "return" expression? ";" ;
- while_statement   -> "while" "(" expression ")" statement ;
- expression        -> assignment ;
- assignment        -> ( call "." )? IDENTIFIER "=" assignment
                     | logic_or ;
- logic_or          -> logic_and ( "or" logic_and )* ;
- logic_and         -> equality ( "and" equality )* ;
- equality          -> comparison ( ("==" | "!=") comparison )*;
- comparison        -> term ( (">" | ">=" | "<" | "<=") term )*;
- term              -> factor ( ("+" | "-") factor )*;
- factor            -> unary ( ("/" | "*") unary )*;
- unary             -> ("!" | "-") unary | call ;
- call              -> primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
- arguments         -> expression ( "," expression )* ;
- primary           -> NUMBER | STRING | "true" | "false" | "nil"
                     | "(" expression ")"
                     | IDENTIFIER 
                     | "super" "." IDENTIFIER ;

```

## Todos

There are a few improvements I would like to see in this project, but for the lack of free time I haven't got around to
them yet.

- [ ] Generate StmtVisitor and ExprVisitor traits using procedural macro. I am not yet familiar enough with 
      proc-macros to do this task efficiently.
- [ ] Currently, all the enums (Stmt, Expr, Literal, etc.) contains fields which is not very ergonomics. Convert their
      inner fields into their own struct.
- [ ] Main CLI for interpreter that can run as REPL environment or with a source file

## Challenges

And lastly, there are challenges in the book that I haven't gotten around to doing yet. The book encourages to do the
challenges in a separate branch since the chapters assume that you have not done any modification (including challenges)
to the code. It would be nice to implement these.

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