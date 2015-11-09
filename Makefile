all:
	ghc --make src/Compiler.hs -o Compiler
clean:
	-rm -f src/*.hi src/*.o
distclean: clean
	-rm -f DocInstant.* LexInstant.* ParInstant.* LayoutInstant.* SkelInstant.* PrintInstant.* TestInstant.* AbsInstant.* TestInstant ErrM.* SharedString.* Instant.dtd XMLInstant.* Makefile*

