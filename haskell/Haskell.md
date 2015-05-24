## Haskell

**Overview**

* Pure Functional programming language
* Strong Static Typed Language 
* Type Inference (The Haskell compiler deduce the types for you). 
* Lazy Evaluation ( Delayed evaluation) by default
* Data Immutability/ Haskell has no variables
    * Values can be bound to a name and can only be assigned once.
    * Values can never change.
* Haskell has not for-loop, while statements.
* Algebraic Data types
* Pattern Matching
* Tail Recursions
* Compiles to native code.


### Suffixes of file names for Haskell

```
.hs    Haskell source code; preprocess, compile

.lhs   literate Haskell source; unlit, preprocess, compile

.hi    Interface file; contains information about exported symbols

.hc    intermediate C files

.x_o   way x object files; common ways are: p, u, s

.x_hi  way x interface files
```


### Toolset

|                                    |                                                      |
|------------------------------------|------------------------------------------------------|
| ghc - the Glasgow Haskell Compiler | Transforms Haskell Source code .hs into native code. |
| ghci                               | Haskell Interactive Shell/ Interpreter               |
| runghc                             | Haskell Non Interactive Interpreter                  | 
| haddock                            | Documentation tool for annotated Haskell source code |
| cabal                              | GHC Haskell Cabal package manager                    |


### Install Haskell Platform

Binaries and Installation files: 
    * https://www.haskell.org/platform/
    * [Haskell for Linux](https://www.haskell.org/platform/linux.html)

Install Haskell Libraries:

```
cabal update

cabal install <some package>
```

<!--
@TODO: Add instructions of how to install  Haskell in Linux.
-->

### GHCI Reference

GHCI Interactive Shell

| Command                     |  Description                                |
|-----------------------------|---------------------------------------------|
| :help                       |  Show help                                  |
| :load [haskell-source.hs] or :l src.hs |    Load Haskell Source Code   |
| :reload or :r                    |  Reload Code after it was edited      |
| :type [symbol]   or :t [symbol]          |  Show the Type of a Symbol   |
| :browse                    |  Gives the type signature of all functions  |
| :set +s                     |  Print timing/memory stats after each evaluation |
| :{ [code here ] :}        |    Multiline Code                            |
| :set prompt ">"             |  Change the prompt to ">"                   |
| :cd [directory]       | change the current working directory to [directory] |
| :! [shell command>]   | execute the shell command; :! pwd  print the current directory |
| :quit                 | Quit the interpreter |


See also:
* [GHCI configuration file](#ghci-configuration-file)
* [GHCI Debugger](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci-debugger.html)
* [GHC and GHCI Man Page](http://manpages.ubuntu.com/manpages/trusty/man1/ghci.1.html)

