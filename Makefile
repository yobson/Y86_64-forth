
FORTH:
	cabal -O2 v2-build;\
	[ -f ./FORTH ] && rm FORTH;\
	find ./dist-newstyle/ -type f -name FORTH -exec cp {} ./ \;

clean:
	[ -f ./FORTH ] && rm FORTH;\
	cabal v2-clean
