all:
	ghc --make src/*.hs -o Compiler
	chmod +x insc_llvm
	chmod +x insc_jvm
clean:
	-rm -f src/*.hi src/*.o
distclean: clean
	-rm -f DocInstant.* LexInstant.* ParInstant.* LayoutInstant.* SkelInstant.* PrintInstant.* TestInstant.* AbsInstant.* TestInstant ErrM.* SharedString.* Instant.dtd XMLInstant.* Makefile*

