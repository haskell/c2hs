#!/bin/sh

prefix=/home/chak/sandbox
exec_prefix=${prefix}
bindir=${exec_prefix}/bin
libdir=${exec_prefix}/lib/c2hs-0.14.0

echo "Moving executable into $libdir/"
/bin/sh install-sh -d $libdir
/bin/sh install-sh -s -m 755 $bindir/c2hs $libdir/

echo "Installing wrapper and C2HS.hs"
/bin/sh install-sh -c -m 755 c2hs/c2hs $bindir/
/bin/sh install-sh -c -m 644 c2hs/lib/C2HS.hs $libdir/
