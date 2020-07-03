
.PHONY: FORTH
FORTH:
	cabal -O2 v2-build;\
	[ -f ./FORTH ] && rm FORTH;\
	find ./dist-newstyle/ -type f -name FORTH -exec cp {} ./ \;

.PHONY: clean
clean:
	cabal v2-clean

.PHONY: clean-all
clean-all:
	[ -f ./FORTH ] && rm FORTH;\
	cabal v2-clean
