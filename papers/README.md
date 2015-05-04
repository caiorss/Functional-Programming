# Haskell Papers

Papers in this repository.

**Why Functional Programming Matters (1984)**

[PDF](Why_Functional_Programming_Matters-John_hughes.pdf)

Abstract

As software becomes more and more complex, it is more and more important to structure it well. Well-structured software is easy to write, easy to debug, and provides a collection of modules that can be re-used to reduce future programming costs. Conventional languages place conceptual limits on the way problems can be modularised. Functional languages push those limits back. In this paper we show that two features of functional languages in particular, higher-order functions and lazy evaluation, can contribute greatly to modularity. As examples, we manipulate lists and trees, program several numerical algorithms, and implement the alphabeta heuristic (an algorithm from Artificial Intelligence used in game-playing programs). Since modularity is the key to successful programming, functional languages are vitally important to the real world.

```
Hughes, John. "Why functional programming matters." The computer journal 32.2 (1989): 98-107.
```

Bibtex
```bibtex
@ARTICLE{Hughes84whyfunctional,
    author = {John Hughes},
    title = {Why Functional Programming Matters},
    journal = {The Computer Journal},
    year = {1984},
    volume = {32},
    pages = {98--107}
}
```


**Composing contracts: an adventure in financial engineering(functional pearl)**

[PDF](Composing_Contracts_an_adventure_in_financial_engineering.pdf)
[ACM Link](http://dl.acm.org/citation.cfm?id=351267)


Abstract:

Financial and insurance contracts do not sound like promising territory for functional programming and formal semantics, but in fact we have discovered that insights from programming languages bear directly on the complex subject of describing and valuing a large class of contracts.We introduce a combinator library that allows us to describe such contracts precisely, and a compositional denotational semantics that says what such contracts are worth. We sketch an implementation of our combinator library in Haskell. Interestingly, lazy evaluation plays a crucial role.


```
Jones, Simon Peyton, Jean-Marc Eber, and Julian Seward. 
"Composing contracts: an adventure in financial engineering(functional pearl).
" ACM SIGPLAN NOTICES 35.9 (2000): 280-292.
```

Bibtex
```
@article{jones2000composing,
  title={Composing contracts: an adventure in financial engineering(functional pearl)},
  author={Jones, Simon Peyton and Eber, Jean-Marc and Seward, Julian},
  journal={ACM SIGPLAN NOTICES},
  volume={35},
  number={9},
  pages={280--292},
  year={2000},
  publisher={ACM; 1999}
}
```
