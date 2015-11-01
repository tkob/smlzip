all: inflate/inflate lszip/lszip catzip/catzip

inflate/inflate:
	cd inflate && make

lszip/lszip:
	cd lszip && make

catzip/catzip:
	cd catzip && make

.PHONY: clean inflate/inflate lszip/lszip catzip/catzip

clean:
	cd inflate && make clean
	cd lszip && make clean
	cd catzip && make clean
