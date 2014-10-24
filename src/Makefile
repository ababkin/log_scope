all: Railoscopy.js

Railoscopy.js: Railoscopy.hs
	hastec Railoscopy.hs # -main-is Railoscopy

StyleGuide.js: StyleGuide.hs
	hastec StyleGuide.hs

clean:
	-rm -r main
	-rm *~
	-rm *.hi
	-rm *.o
	-rm Railoscopy.js
	-rm StyleGuide.js

distclean: clean
	-rm Railoscopy
	-rm Railoscopy.js
