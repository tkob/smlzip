M4FLAGS = -DSML_COMPILER=mlton

all: catzip

catzip: catzip.sml catzip.mlb
	mlton catzip.mlb

catzip.sml: catzip.sml.in
	m4 $(M4FLAGS) $< > $@

clean:
	rm -f catzip catzip.sml
