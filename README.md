# Platy Programming Language

Platy is a small programming language for practice of creating language. The compiler is a [LLVM](https://llvm.org/)-based compiler.

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
(@func fib [(:: n Int)] Int
    (@if (or [(eq-int [n, 0]), (eq-int [n, 1])])
        1
        (add-int [(fib [(sub-int [n, 2])]), (fib [(sub-int [n, 1])])])
    )
)

(@func main [] Unit
    (@let [
        ; Calculate fib(1)
        (= fib1  Int (fib [1])),
        ; Print it
        (= _1 Unit (print-int [fib1])),

        ; Calculate fib(30)
        (= fib30 Int (fib [30])),
        ; Print it
        (= _2 Unit (print-int [fib30]))
    ] Unit)
)
```

You can fild whole code in [platy_programs/should_pass/fib.platy](platy_programs/should_pass/fib.platy)

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
