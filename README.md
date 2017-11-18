# Project Functional programming 16-17


## Usage
```
$ runhaskell parser.hs
```


## BNF
### Arithmetic Expression
```
AExp    := AExp + ATerm | AExp - ATerm | ATerm
ATerm   := ATerm * AFactor | ATerm / AFactor | AFactmor
AFactor := (AExp) | Const | Variable
```

### Boolean Expression
```
BExp    := BExp || BExp | BTerm
BTerm   := BTerm && BTerm | BFactor
BFactor := (BExp) | ! BFactor | Const | Variable | BRel
BRel    := AExp < AExp | AExp == AExp | AExp > AExp
```
