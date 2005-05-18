include ../../mk/config.mk

HCFLAGS= -package lang -fglasgow-exts -i../lib -L../lib -lc2hs
C2HS   = ../c2hs

PRGMS = simple calls enums pointer structs marsh cpp

default: tests

# builds

simple: Simple.chs simple.h simple.c
	$(C2HS) simple.h Simple.chs
	$(HC) -c -o Simple.o Simple.hs $(HCFLAGS)
	$(CC) -c simple.c
	$(HC) -o simple simple.o Simple.o

calls: Calls.chs calls.h
	$(C2HS) calls.h Calls.chs
	$(HC) -c -o Calls.o Calls.hs -#include\"calls.h\" $(HCFLAGS)

enums: Enums.chs enums.h
	$(C2HS) enums.h Enums.chs
	$(CC) -c enums.c
	$(HC) -o enums enums.o Enums.hs -#include\"enums.h\" $(HCFLAGS)

pointer: Pointer.chs pointer.h pointer.c
	$(C2HS) pointer.h Pointer.chs
	$(CC) -c pointer.c
	$(HC) -o pointer pointer.o Pointer.hs -#include\"pointer.h\" $(HCFLAGS)

structs: Structs.chs structs.h structs.c
	$(C2HS) structs.h Structs.chs
	$(HC) -c -o Structs.o Structs.hs -#include\"structs.h\" $(HCFLAGS)
	$(CC) -c structs.c
	$(HC) -o structs structs.o Structs.o $(HCFLAGS)

marsh: Marsh.chs marsh.h
	$(C2HS) marsh.h Marsh.chs
	$(HC) -o marsh Marsh.hs -#include\"marsh.h\" $(HCFLAGS)

cpp: Cpp.chs cpp.h
	$(C2HS) Cpp.chs
	$(HC) -c -o Cpp.o Cpp.hs -#include\"Cpp.h\" $(HCFLAGS)

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
