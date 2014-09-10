all: app app.js

app.js: app.hs
	hastec src/app.hs

app:
	ghc --make src/app.hs

clean:
	-rm -r main
	-rm *~
	-rm app.hi
	-rm app.o

distclean: clean
	-rm app
	-rm app.js
