`c2hs` is a interfacing tool that eases Haskell access to C libraries.
The tool gets information about the C data type definitions and
function signatures by analysing the C header files of the library.
It uses this information to compute the missing details in the
template of a Haskell module &mdash; called the binding file &mdash;
that implements a Haskell binding to the C library.  Hooks embedded in
the binding file signal where, which, and how C objects are accessed
from Haskell.  The Haskell code in the binding file determines Haskell
types signatures and marshaling details.

Further information is on the
[wiki](https://github.com/haskell/c2hs/wiki/Home).  Also see the
[user guide](https://github.com/haskell/c2hs/wiki/User-Guide) (also
available in the `doc` directory of the repository).


## Installing

See the file `INSTALL`.


## Copyleft

This system is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

This system is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this system; if not, write to the Free Software Foundation,
Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

> **NOTE HOWEVER**, all code included into generated bindings is under
> a BSD-style license that does not place any restrictions on the
> license of the inteface produced with `c2hs` (ie, closed proprietary
> licenses are possible, too).  In other words, I do not care what you
> use `c2hs` for or to whom you are giving `c2hs` or any interfaces
> generated with `c2hs`, only if you modify or improve `c2hs` itself,
> you have to contribute your changes back to the community.
> Nevertheless, I will of course be particularly delighted if you
> choose to make your work freely available.


## Credits

See the file `AUTHORS`.
