OBJ = low.cmo test.cmo

test: $(OBJ) pa.cmo
	ocamlc $(OBJ) -o $@

pa.cmo: pa.ml5
	ocamlc -c -I +camlp5 -pp "camlp5o -impl" camlp5.cma -impl $^

%.cmo: %.ml pa.cmo
	ocamlc -pp "camlp5o ./pa.cmo" -c $<

clean:
	rm -f *.cm? *.o *.out test

.PHONY: clean

# Dependencies between modules, crafted with love
test.cmo: low.cmo
