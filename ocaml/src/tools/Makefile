main.byte:
	ocamlfind ocamlc -o main.byte tools.ml main.ml  -package unix -linkpkg -verbose

main.bin:
	ocamlfind ocamlopt -o main.bin tools.ml main.ml  -package unix -linkpkg -verbose
    
clean:
	rm -rf *.cmo *.cmi *.o *.cmx *.bin *.byte
