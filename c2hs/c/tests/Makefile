
all : parseHeader parseUnit cc-wrapper

parseHeader: ParseHeader.hs ../CParser.hs ../CLexer.hs CPP.hs GCC.hs
	ghc --make -o $@ $< -i$(SEARCHPATH)

parseUnit: ParseUnit.hs ../CParser.hs ../CLexer.hs
	ghc --make -o $@ $< -i$(SEARCHPATH)

cc-wrapper: CCWrapper.hs ../CParser.hs ../CLexer.hs CPP.hs
	ghc --make -O -o $@ $< -i$(SEARCHPATH)

%.hs: %.y
	happy -s -g -c -a -o $@ $<

%.hs: %.x
	alex -g -o $@ $<

SEARCHPATH=..:../../../base/admin:../../../base/general:../../../base/syms:../../../base/errors:../../../base/state:../../state
