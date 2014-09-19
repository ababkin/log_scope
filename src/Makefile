all: Main Main.js

Main.js: Main.hs
	hastec Main.hs

Main:
	ghc --make Main.hs

clean:
	-rm -r main
	-rm *~
	-rm Main.hi
	-rm Main.o

distclean: clean
	-rm Main
	-rm Main.js
