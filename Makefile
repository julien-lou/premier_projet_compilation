INTERFACE := readfile.cmi x86_64.cmi lexer.cmi asyntax.cmi parser.cmi typecheck.cmi asmeditor.cmi
MAIN_OBJS := readfile.cmo x86_64.cmo lexer.cmo asyntax.cmo parser.cmo typecheck.cmo asmeditor.cmo main.cmo


all: aritha rapport.pdf

rapport.pdf:
	pdflatex -shell-escape rapport.tex

executeOcaml:
	./aritha

aritha: $(INTERFACE) $(MAIN_OBJS)
	ocamlc -o aritha $(MAIN_OBJS)

%.cmi: %.mli
	ocamlc -c $< -o $@
%.cmo: %.ml
	ocamlc -c $< -o $@

x86_64.cmi: x86_64.mli
lexer.cmi: lexer.mli
asyntax.cmi: asyntax.mli
parser.cmi: parser.mli
typecheck.cmi: typecheck.mli
asmeditor.cmi: asmeditor.mli
readfile.cmi: readfile.mli

x86_64.cmo: x86_64.ml
lexer.cmo: lexer.ml
asyntax.cmo: asyntax.ml
parser.cmo: parser.ml
typecheck.cmo: typecheck.ml
asmeditor.cmo: asmeditor.ml
main.cmo: main.ml
readfile.cmo: readfile.ml	

clean:
	rm -f aritha expression *.cmi *.cmo *.s

