all : executable

generate : executable generate.cpp
	g++-5 -std=c++14 -Wall -Wextra -pedantic -O4 -static generate.cpp -o generate

executable : leftist.cmx test.cmx
	ocamlopt -S leftist.cmx test.cmx -o executable

leftist.cmx : leftist.cmi leftist.ml
	ocamlopt -S -c leftist.ml

test.cmx : leftist.cmi leftist.ml test.ml
	ocamlopt -S -c test.ml

leftist.cmi : leftist.mli
	ocamlopt leftist.mli

clean :
	rm *.s *.o *.cmx *.cmi executable 
	if [ -f "generate" ]; then rm generate test.in test.out; fi
	if [ -f "answer.out" ]; then rm answer.out; fi
