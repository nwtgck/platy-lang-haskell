# Platy Programming Language

Platy is a small programming language for practice of creating language. The compiler is a [LLVM](https://llvm.org/)-based compiler.

It was designed to realize the following three ideas.

1. **Declarative**
1. **Statically typed**
1. **Simple to implement**


| branch | Travis status|
| --- | --- |
| [`master`](https://github.com/nwtgck/platy-lang-haskell/tree/master) | [![Build Status](https://travis-ci.com/nwtgck/platy-lang-haskell.svg?token=TuxNpqznwwyy7hyJwBVm&branch=master)](https://travis-ci.com/nwtgck/platy-lang-haskell) |
| [`develop`](https://github.com/nwtgck/platy-lang-haskell/tree/develop) | [![Build Status](https://travis-ci.com/nwtgck/platy-lang-haskell.svg?token=TuxNpqznwwyy7hyJwBVm&branch=develop)](https://travis-ci.com/nwtgck/platy-lang-haskell) |

## Installation of the compiler

```bash
stack build
stack install
```

Then you can use `platyc` command.

## Fibonacci function in Platy

![fib gif](demo_images/fib.gif)

```clojure
                                                                        ; [Pseudo code]
                                                                        ;
(@func fib [(:: n Int)] Int                                             ; func fib(n :: Int) -> Int =
    (@if (or [(eq-int [n, 0]), (eq-int [n, 1])])                        ;   if (n == 0 || n == 1)
        1                                                               ;     then 1
        (add-int [(fib [(sub-int [n, 2])]), (fib [(sub-int [n, 1])])])  ;     else fib (n - 2) + fib (n - 1)
    )                                                                   ;
)                                                                       ;
                                                                        ;
(@func main [] Unit                                                     ; func main() -> Unit =
    (@let [                                                             ;   let
        ; Calculate fib(1)                                              ;     // Calculate fib(1)
        (= fib1  Int (fib [1])),                                        ;     fib1 :: Int   = fib(1)
        ; Print it                                                      ;     // Print it
        (= _1 Unit (print-int [fib1])),                                 ;     _1   :: Unit  = print_int(fib1)
                                                                        ;
        ; Calculate fib(30)                                             ;     // Calculate fib(30)
        (= fib30 Int (fib [30])),                                       ;     fib30 :: Int  = fib 30
        ; Print it                                                      ;     // Print it
        (= _2 Unit (print-int [fib30]))                                 ;     _2    :: Unit = print_int(fib30)
    ] Unit)                                                             ;   in Unit
)
```

You can find actual code in [platy_programs/should_pass/fib.platy](platy_programs/should_pass/fib.platy).

## Features

* Declarative
* Statically typed
* Strict evaluation (not lazy evalluation)
* Impure function
* Non-functional


### What Platy haves
* Variable binding
* Function definition
* `Int`
* `Char`
* `Bool`
* `Unit`


### What Platy doesn't have
* Lambda expression
* Struct
* `String` type
* Assignment
* Object-oriented (no method, no polymorphism)
* Type class, interface, trait
* Package, module
* Union type
* Garbage collection
* Type alias


## Why S-expression-like syntax?

Because it keeps easy to create parser. In addition, S-expression is very scalable and expandable.