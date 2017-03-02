FSI=fsharpi # F# Interpreter path 

all:
	fsharpi build.fsx	

clean:
	# Remove all temporary files
	find . -name "*~" | xargs rm -rf
    # Remove all *.html files
	find . -name "*.html" | xargs rm -rf


upload:
	git push github master

