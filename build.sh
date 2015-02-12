#!/bin/bash
#
#   Pandoc Markdown and ReST Compared (2013) 
#   https://news.ycombinator.com/item?id=7364937
#
#----------------------------------------------------------------------



#doctoc LearnHaskell.md

# Build PDF documentation
#pandoc --latex-engine=xelatex -f markdown_github -o LearnHaskell.pdf LearnHaskell.md


grip LearnHaskell.md --gfm --export LearnHaskell2.html

# Build HTML documentation
#pandoc -f markdown_github -o LearnHaskell.html LearnHaskell.md --standalone 

pandoc -f markdown_github -o LearnHaskell.html LearnHaskell.md --highlight-style=tango --toc --standalone 

echo $(pwd)
