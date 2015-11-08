all:
	ghc --make Compiler.hs -o Compiler
clean:
	-rm -f *.log *.aux *.hi *.o *.dvi
	-rm -f DocInstant.ps
distclean: clean
	-rm -f DocInstant.* LexInstant.* ParInstant.* LayoutInstant.* SkelInstant.* PrintInstant.* TestInstant.* AbsInstant.* TestInstant ErrM.* SharedString.* Instant.dtd XMLInstant.* Makefile*

