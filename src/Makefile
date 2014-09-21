all: LogScope.js

LogScope.js: LogScope.hs
	hastec LogScope.hs -main-is LogScope

clean:
	-rm -r main
	-rm *~
	-rm LogScope.hi
	-rm LogScope.o
	-rm LogScope.js

distclean: clean
	-rm LogScope
	-rm LogScope.js
