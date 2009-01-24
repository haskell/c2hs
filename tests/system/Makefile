# Note that file.o and File.o are the same file on case-insensitive systems
# Therefore we shouldn't use one for C and one for HS
HC=ghc

HCFLAGS= -fffi
C2HS   = ../../dist/build/c2hs/c2hs
# C2HSFLAGS = -d trace -d genbind -d ctrav -d chs
PRGMS = simple calls enums pointer structs marsh cpp

default: tests

# builds

C2HS.o: ../../C2HS.hs
	cp -p ../../C2HS.hs .
	$(HC) -c C2HS.hs

simple: C2HS.o Simple.chs simple.h simple.c
	$(C2HS) $(C2HSFLAGS) simple.h Simple.chs
	$(HC) -c -o Simple_hs.o Simple.hs $(HCFLAGS)
	$(CC) -c simple.c
	$(HC) -o simple simple.o Simple_hs.o C2HS.o

calls: C2HS.o Calls.chs calls.h
	$(C2HS) $(C2HSFLAGS) calls.h Calls.chs
	$(HC) -c -o Calls.o Calls.hs -#include\"calls.h\" $(HCFLAGS) || \
	echo "!!! Building calls failed ! known bug #?"

enums: C2HS.o Enums.chs enums.h
	$(C2HS) $(C2HSFLAGS) enums.h Enums.chs
	$(CC) -o enums_c.o -c enums.c
	$(HC) -o enums enums_c.o Enums.hs -#include\"enums.h\" $(HCFLAGS) C2HS.o

pointer: C2HS.o Pointer.chs pointer.h pointer.c
	$(C2HS) $(C2HSFLAGS) pointer.h Pointer.chs
	$(CC) -o pointer_c.o -c pointer.c
	$(HC) -o pointer pointer_c.o Pointer.hs -#include\"pointer.h\"\
	  $(HCFLAGS) C2HS.o

sizeof: C2HS.o Sizeof.chs sizeof.h sizeof.c
	$(C2HS) $(C2HSFLAGS) sizeof.h Sizeof.chs
	$(HC) -c -o Sizeof.o Sizeof.hs -#include\"sizeof.h\" $(HCFLAGS)
	$(CC) -o sizeof_c.o -c sizeof.c
	$(HC) -o sizeof sizeof_c.o Sizeof.o $(HCFLAGS) C2HS.o

structs: C2HS.o Structs.chs structs.h structs.c
	$(C2HS) $(C2HSFLAGS) structs.h Structs.chs
	$(HC) -c -o Structs.o Structs.hs -#include\"structs.h\" $(HCFLAGS)
	$(CC) -o structs_c.o -c structs.c
	$(HC) -o structs structs_c.o Structs.o $(HCFLAGS) C2HS.o

marsh: C2HS.o Marsh.chs marsh.h
	$(C2HS) $(C2HSFLAGS) marsh.h Marsh.chs
	$(HC) -o marsh Marsh.hs -#include\"marsh.h\" $(HCFLAGS) C2HS.o

cpp: C2HS.o Cpp.chs cpp.h
	$(C2HS) $(C2HSFLAGS) Cpp.chs
	$(HC) -c -o Cpp.o Cpp.hs -#include\"Cpp.h\" $(HCFLAGS) C2HS.o

# runs

.PHONY: tests simple.run calls.build enums.run pointer.run structs.run\
	marsh.run cpp.build

tests: simple.run enums.run pointer.run structs.run marsh.run\
       cpp.build buggy
buggy: calls.build sizeof.run

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

sizeof.run: sizeof
	@echo "---=== Output for \`sizeof'":
	@./sizeof || \
	echo "!!! sizeof FAILED: Maybe related to bug #10"
	@echo "---=== End of Output"


structs.run: structs
	@echo "---=== Output for \`structs'":
	@./structs > structs.out
	@cat structs.out
	@diff structs.out structs.expect
	@echo "---=== End of Output (diff ok)"

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
	-rm -f *.o *.hi *.hs *.out $(PRGMS)
