all: curl.byte curl.bin

curl.byte:
	ocamlfind ocamlc -o curl.byte curl.ml -package uri,lwt,lwt.unix,cohttp.lwt -linkpkg -pp 'sed "s/^#.*;;//"'

curl.bin:
	ocamlfind ocamlopt -o curl.bin curl.ml -package uri,lwt,lwt.unix,cohttp.lwt -linkpkg -pp 'sed "s/^#.*;;//"'

test: test.byte test.bin

test.byte:
	@echo "Testing curl.byte"
	./curl.byte http://httpbin.org/status/418

test.bin:
	@echo "Testing curl.bin"
	./curl.byte http://httpbin.org/status/418

clean: 
	rm -rf *.cmo *.cmi *.o *.cmx *.bin *.byte
