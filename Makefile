FSI=fsharpi # F# Interpreter path 

all:
	fsharpi build.fsx	


show:
	firefox dist/index.html

upload:
	fsharpi build.fsx
	cd dist && git commit -a -m "Update gh-pages" && git push 


clean:
	# Remove all temporary files
	find . -name "*~" | xargs rm -rf
    # Remove all *.html files
	find . -name "*.html" | xargs rm -rf



