
index:
	doctoc  README.md

html:
	grip README.md --gfm --export ./README.html

tags:
	git tag

clean:
	rm -rf *.hi *.o
	
     
upload:
	git push github master

commit:
	git commit 
