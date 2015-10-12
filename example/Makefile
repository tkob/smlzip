M4FLAGS = -DSML_COMPILER=mlton

all: inflate

inflate: inflate.sml inflate.mlb
	mlton inflate.mlb

inflate.sml: inflate.sml.in
	m4 $(M4FLAGS) $< > $@

clean:
	rm -f inflate inflate.sml
