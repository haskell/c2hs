ifeq ($(strip $(shell pwd | grep build)),)
  *** If you get an error here, 
  *** you did not execute make in the *build* tree (but somewhere else)!
endif


HC     = ghc
HCFLAGS= -fglasgow-exts -i../lib -L../lib -lc2hs
C2HS   = ../c2hs

PRGMS = simple calls enums structs marsh

default: tests

# builds

simple: Simple.chs simple.h simple.c
	$(C2HS) simple.h Simple.chs
	$(HC) -c Simple.hs $(HCFLAGS)
	$(CC) -c simple.c
	$(HC) -o simple simple.o Simple.o

calls: Calls.chs calls.h
	$(C2HS) calls.h Calls.chs
	$(HC) -c Calls.hs -#include\"calls.h\" $(HCFLAGS)

enums: Enums.chs enums.h
	$(C2HS) enums.h Enums.chs
	$(HC) -o enums Enums.hs -#include\"enums.h\" $(HCFLAGS)

structs: Structs.chs structs.h structs.c
	$(C2HS) structs.h Structs.chs
	$(HC) -c Structs.hs -#include\"structs.h\" $(HCFLAGS)
	$(CC) -c structs.c
	$(HC) -o structs structs.o Structs.o $(HCFLAGS)

marsh: Marsh.chs marsh.h
	$(C2HS) marsh.h Marsh.chs
	$(HC) -o marsh Marsh.hs -#include\"marsh.h\" $(HCFLAGS)

# runs

.PHONY: tests simple.run calls.build enums.run structs.run marsh.run

tests: simple.run calls.build enums.run structs.run marsh.run

simple.run: simple
	@echo "---=== Output of \`simple'":
	@simple
	@echo "---=== End of Output"

calls.build: calls
	@echo "---=== Binding for \`calls'":
	@cat Calls.hs
	@echo "---=== End of Binding"

enums.run: enums
	@echo "---=== Output for \`enums'":
	@enums
	@echo "---=== End of Output"

structs.run: structs
	@echo "---=== Output for \`structs'":
	@structs
	@echo "---=== End of Output"

marsh.run: marsh
	@echo "---=== Output for \`marsh'":
	@marsh
	@echo "---=== End of Output"

# misc

clean:
	-rm -f *.o *.hi *.hs $(PRGMS)
