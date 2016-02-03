- [Haskell](#haskell)
  - [Overview](#overview)
  - [Suffixes of file names for Haskell](#suffixes-of-file-names-for-haskell)
  - [Toolset](#toolset)
  - [Install Haskell Platform](#install-haskell-platform)
  - [GHCI Reference](#ghci-reference)


# Haskell<a id="sec-1" name="sec-1"></a>



## Overview<a id="sec-1-1" name="sec-1-1"></a>

-   Pure Functional programming language
-   Strong Static Typed Language
-   Type Inference (The Haskell compiler deduce the types for you).
-   Lazy Evaluation ( Delayed evaluation) by default
-   Data Immutability/ Haskell has no variables
    -   Values can be bound to a name and can only be assigned once.
    -   Values can never change.
-   Haskell has not for-loop, while statements.
-   Algebraic Data types
-   Pattern Matching
-   Tail Recursions
-   Compiles to native code.

## Suffixes of file names for Haskell<a id="sec-1-2" name="sec-1-2"></a>

```
.hs    Haskell source code; preprocess, compile

.lhs   literate Haskell source; unlit, preprocess, compile

.hi    Interface file; contains information about exported symbols

.hc    intermediate C files

.x_o   way x object files; common ways are: p, u, s

.x_hi  way x interface files
```

## Toolset<a id="sec-1-3" name="sec-1-3"></a>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">&#xa0;</th>
<th scope="col" class="left">&#xa0;</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">ghc - the Glasgow Haskell Compiler</td>
<td class="left">Transforms Haskell Source code .hs into native code.</td>
</tr>


<tr>
<td class="left">ghci</td>
<td class="left">Haskell Interactive Shell/ Interpreter</td>
</tr>


<tr>
<td class="left">runghc</td>
<td class="left">Haskell Non Interactive Interpreter</td>
</tr>


<tr>
<td class="left">haddock</td>
<td class="left">Documentation tool for annotated Haskell source code</td>
</tr>


<tr>
<td class="left">cabal</td>
<td class="left">GHC Haskell Cabal package manager</td>
</tr>
</tbody>
</table>

## Install Haskell Platform<a id="sec-1-4" name="sec-1-4"></a>

Binaries and Installation files: 
-   <https://www.haskell.org/platform/>
-   [Haskell for Linux](https://www.haskell.org/platform/linux.html)

Install Haskell Libraries:

```
cabal update

cabal install <some package>
```

## GHCI Reference<a id="sec-1-5" name="sec-1-5"></a>

GHCI Interactive Shell

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">Command</th>
<th scope="col" class="left">Description</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">:help</td>
<td class="left">Show help</td>
</tr>


<tr>
<td class="left">:load [haskell-source.hs] or :l src.hs</td>
<td class="left">Load Haskell Source Code</td>
</tr>


<tr>
<td class="left">:reload or :r</td>
<td class="left">Reload Code after it was edited</td>
</tr>


<tr>
<td class="left">:type [symbol]   or :t [symbol]</td>
<td class="left">Show the Type of a Symbol</td>
</tr>


<tr>
<td class="left">:browse</td>
<td class="left">Gives the type signature of all functions</td>
</tr>


<tr>
<td class="left">:set +s</td>
<td class="left">Print timing/memory stats after each evaluation</td>
</tr>


<tr>
<td class="left">:{ [code here ] :}</td>
<td class="left">Multiline Code</td>
</tr>


<tr>
<td class="left">:set prompt ">"</td>
<td class="left">Change the prompt to ">"</td>
</tr>


<tr>
<td class="left">:cd [directory]</td>
<td class="left">change the current working directory to [directory]</td>
</tr>


<tr>
<td class="left">:! [shell command>]</td>
<td class="left">execute the shell command; :! pwd  print the current directory</td>
</tr>


<tr>
<td class="left">:quit</td>
<td class="left">Quit the interpreter</td>
</tr>
</tbody>
</table>

See also:

-   GHCI configuration file (See section )
-   [GHCI Debugger](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci-debugger.html)
-   [GHC and GHCI Man Page](http://manpages.ubuntu.com/manpages/trusty/man1/ghci.1.html)
