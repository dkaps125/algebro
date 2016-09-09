all:
	ocamlopt -o algebro str.cmxa scheme.ml

clean:
	rm scheme.cm* && rm scheme.o && rm algebro
