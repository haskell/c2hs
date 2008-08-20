C2HS_ROOT=../../../
C2HS_BIN=$(C2HS_ROOT)/dist/build/c2hs/c2hs
C2HS_FLAGS=-d genbind
C2HS=$(C2HS_BIN) $(C2HS_FLAGS)

HC_BIN=ghc
HC_FLAGS=--make -fffi -cpp
HC=$(HC_BIN) $(HC_FLAGS)

# generic rule when .h, .c, .chs is given

% :: %.h %.c %.chs
	$(C2HS) $@.h $@.chs
	$(CC) -c -o $@_c.o $@.c
	$(HC) $@_c.o $@.hs
	./$@ > $@.out
	diff -u $@.expect $@.out
%.clean :: %.chs clean_tmp_files
	rm -rf $(subst .clean,,$@) $(subst .clean,.hs,$@) $(subst .clean,.chs.h,$@)
clean_tmp_files :
	rm -rf *.o *.hi *.out *.chi

