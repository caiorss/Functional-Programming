all:
	scala -save build.scala --make all

show:
	firefox dist/index.html

upload:
	scala -save build.scala --make all
	cd dist && git commit -a -m "Update gh-pages" && git push 

clean:
	scala -save build.scala -clean 


