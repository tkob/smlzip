all: check

check: example/inflate t/seqmatch.t t/pkzip.t
	runtest
	prove --exec t/do-test

example/inflate:
	cd example && make

.PHONY: clean example/inflate

clean:
	cd example && make clean
