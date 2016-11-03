
html:
	emacs --batch  -l build.el --kill 

tags:
	git tag

clean:
	rm -rf  {.,dist}/{clojure,ocaml,haskell}/{.*html,.*html~}

upload:
	git push github master

commit:
	git commit 
