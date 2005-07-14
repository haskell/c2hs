HC=ghc

HCFLAGS= -ffi
C2HS   = ../c2hs-inplace

PRGMS = simple calls enums pointer structs marsh cpp

default: tests

# builds

C2HS.o: ../lib/C2HS.hs
	$(C2HS) --copy-library
	$(HC) -c C2HS.hs

simple: C2HS.o Simple.chs simple.h simple.c
	$(C2HS) simple.h Simple.chs
	$(HC) -c -o Simple.o Simple.hs $(HCFLAGS)
	$(CC) -c simple.c
	$(HC) -o simple simple.o Simple.o C2HS.o

calls: C2HS.o Calls.chs calls.h
	$(C2HS) calls.h Calls.chs
	$(HC) -c -o Calls.o Calls.hs -#include\"calls.h\" $(HCFLAGS)

enums: C2HS.o Enums.chs enums.h
	$(C2HS) enums.h Enums.chs
	$(CC) -c enums.c
	$(HC) -o enums enums.o Enums.hs -#include\"enums.h\" $(HCFLAGS) C2HS.o

pointer: C2HS.o Pointer.chs pointer.h pointer.c
	$(C2HS) pointer.h Pointer.chs
	$(CC) -c pointer.c
	$(HC) -o pointer pointer.o Pointer.hs -#include\"pointer.h\"\
	  $(HCFLAGS) C2HS.o

structs: C2HS.o Structs.chs structs.h structs.c
	$(C2HS) structs.h Structs.chs
	$(HC) -c -o Structs.o Structs.hs -#include\"structs.h\" $(HCFLAGS)
	$(CC) -c structs.c
	$(HC) -o structs structs.o Structs.o $(HCFLAGS) C2HS.o

marsh: C2HS.o Marsh.chs marsh.h
	$(C2HS) marsh.h Marsh.chs
	$(HC) -o marsh Marsh.hs -#include\"marsh.h\" $(HCFLAGS) C2HS.o

cpp: C2HS.o Cpp.chs cpp.h
	$(C2HS) Cpp.chs
	$(HC) -c -o Cpp.o Cpp.hs -#include\"Cpp.h\" $(HCFLAGS) C2HS.o

# runs

.PHONY: tests simple.run calls.build enums.run pointer.run structs.run\
	marsh.run cpp.build

tests: simple.run calls.build enums.run pointer.run structs.run marsh.run\
       cpp.build

simple.run: simple
	@echo "---=== Output of \`simple'":
	@./simple
	@echo "---=== End of Output"

calls.build: calls
	@echo "---=== Binding for \`calls'":
	@cat Calls.hs
	@echo "---=== End of Binding"

enums.run: enums
	@echo "---=== Output for \`enums'":
	@./enums
	@echo "---=== End of Output"

pointer.run: pointer
	@echo "---=== Output for \`pointer'":
	@./pointer
	@echo "---=== End of Output"

structs.run: structs
	@echo "---=== Output for \`structs'":
	@./structs
	@echo "---=== End of Output"

marsh.run: marsh
	@echo "---=== Output for \`marsh'":
	@./marsh
	@echo "---=== End of Output"

cpp.build: cpp
	@echo "---=== Binding for \`cpp'":
	@cat Cpp.hs
	@echo "---=== End of Binding"

# misc

clean:
	-rm -f *.o *.hi *.hs $(PRGMS)
