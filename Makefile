# Make sure ocamlbuild can find opam-managed packages: first run
#
# eval `opam config env`

# Easiest way to build: using ocamlbuild, which in turn uses ocamlfind

.PHONY : all
all : strux.native 

.PHONY : strux.native
strux.native :
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis,llvm.linker,llvm.bitreader,llvm.irreader -cflags -w,+a-4 \
		strux.native
	./linkStrux.sh

# "make clean" removes all generated files

.PHONY : clean
clean :
	ocamlbuild -clean
	rm -rf testall.log *.diff strux scanner.ml parser.ml parser.mli
	rm -rf printbig stack queue linkedlist
	rm -rf *.cmx *.cmi *.cmo *.cmx *.o *.s *.ll *.out *.exe *.bc ./*.err

OBJS = ast.cmx codegen.cmx parser.cmx scanner.cmx semant.cmx strux.cmx

YACC = ocamlyacc

strux: $(OBJS)
	ocamlfind ocamlopt -linkpkg -package llvm -package llvm.analysis $(OBJS) -o strux

scanner.ml: scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli: parser.mly
	$(YACC) parser.mly

%.cmo : %.ml
	ocamlc -c $<

%.cmi : %.mli
	ocamlc -c $<

%.cmx : %.ml
	ocamlfind ocamlopt -c -package llvm $<

### Generated by "ocamldep *.ml *.mli" after building scanner.ml and parser.ml
ast.cmo :
ast.cmx :
codegen.cmo : ast.cmo queue.bc linkedlist.bc stack.bc bstree.bc
codegen.cmx : ast.cmx queue.bc linkedlist.bc stack.bc bstree.bc
strux.cmo : semant.cmo scanner.cmo parser.cmi codegen.cmo ast.cmo
strux.cmx : semant.cmx scanner.cmx parser.cmx codegen.cmx ast.cmx
parser.cmo : ast.cmo parser.cmi
parser.cmx : ast.cmx parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx
semant.cmo : ast.cmo
semant.cmx : ast.cmx
parser.cmi : ast.cmo

# Building the tarball

TESTS = *
FAILS = *
C = *
H = *

TESTFILES = $(TESTS:%=test-%.strux) $(TESTS:%=test-%.out) \
	    $(FAILS:%=fail-%.strux) $(FAILS:%=fail-%.err)

CFILES = $(C:%=%.c) $(H:%=%.h)

TARFILES = ast.ml codegen.ml Makefile strux.ml parser.mly README.md \
	   scanner.mll semant.ml linkStrux.sh testall.sh \
	   $(CFILES:%=c/%) $(TESTFILES:%=tests/%)

strux.tar.gz : $(TARFILES)
	cd .. && tar czf strux/strux.tar.gz \
		$(TARFILES:%=strux/%)
