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

### Simple rule table

```
- expression -> literal
              | unary
              | binary
              | grouping 
              | assignment ;
- assignment -> IDENTIFIER "=" assignment 
              | equality;
- literal    -> NUMBER | STRING | "true" | "false" | "nil" ;
- grouping   -> "(" expression ")" ;
- unary      -> ( "-" | "!" ) expression ;
- binary     -> expression operator expression ;
- operator   -> "==" | "!=" | "<" | "<=" | ">" | ">="
              | "+" | "-"  | "*" | "/" ;
```