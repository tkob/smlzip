M4FLAGS = -DSML_COMPILER=mlton

all: lszip

lszip: lszip.sml lszip.mlb ../../binrandomaccessfile.sml ../../seqmatch.sml ../../deflate.sml ../../pkzip.sml
	mlton lszip.mlb

lszip.sml: lszip.sml.in
	m4 $(M4FLAGS) $< > $@

clean:
	rm -f lszip lszip.sml
