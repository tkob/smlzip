all: check

check: example/inflate t/seqmatch.t t/pkzip.t
	runtest
	t/do-test t/seqmatch.t t/pkzip.t

example/inflate:
	cd example && make

.PHONY: clean example/inflate

clean:
	cd example && make clean
