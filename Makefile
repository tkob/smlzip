all: check

check: example/inflate
	runtest

example/inflate:
	cd example && make

.PHONY: clean example/inflate

clean:
	cd example && make clean
