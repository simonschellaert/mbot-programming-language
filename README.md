# Project Functional programming 16-17


## Usage
```
$ runhaskell parser.hs
```


## Backus-Naur Form
### Arithmetic Expressions
```
Exp    := Exp + Term | Exp - Term | Term
Term   := Term * Factor | Term / Factor | Factor
Factor := (Exp) | Const | Variable
```


