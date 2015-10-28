all: inflate/inflate lszip/lszip

inflate/inflate:
	cd inflate && make

lszip/lszip:
	cd lszip && make

.PHONY: clean inflate/inflate lszip/lszip

clean:
	cd inflate && make clean
	cd lszip && make clean
