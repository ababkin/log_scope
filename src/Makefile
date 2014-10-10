all: Railoscopy.js

LogScope.js: Railoscopy.hs
	hastec Railoscopy.hs -main-is Railoscopy

clean:
	-rm -r main
	-rm *~
	-rm Railoscopy.hi
	-rm Railoscopy.o
	-rm Railoscopy.js

distclean: clean
	-rm Railoscopy
	-rm Railoscopy.js
