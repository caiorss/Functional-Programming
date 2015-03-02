#!/bin/bash
#
#   Pandoc Markdown and ReST Compared (2013) 
#   https://news.ycombinator.com/item?id=7364937
#
#----------------------------------------------------------------------



doctoc README.md

# Build PDF documentation
#pandoc --latex-engine=xelatex -f markdown_github -o LearnHaskell.pdf LearnHaskell.md


grip README.md --gfm --export LearnHaskell.html

# Build HTML documentation
#pandoc -f markdown_github -o LearnHaskell.html LearnHaskell.md --standalone 

# pandoc -f markdown_github -o LearnHaskell.html LearnHaskell.md --highlight-style=tango --toc 

echo $(pwd)
