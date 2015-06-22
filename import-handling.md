## Potentially breaking changes: import handling

#### The problem

Previous releases of C2HS had an annoying misfeature -- you had to
manage the imports of Haskell library functions in C2HS-generated code
yourself.  Suppose you had the following code in a `.chs` file:

``` haskell
#include "issue44.h"

{#pointer *foo as ^ foreign newtype#}
```

where the contents of the `issue44.h` header are:

``` c
typedef struct { int a; } foo;
```

Running C2HS would then generate the following Haskell code:

``` haskell
newtype Foo = Foo (ForeignPtr (Foo))
withFoo :: Foo -> (Ptr Foo -> IO b) -> IO b
withFoo (Foo fptr) = withForeignPtr fptr
```

Note the use of the names `ForeignPtr`, `Ptr` and `withForeignPtr`.
These come from the Haskell library modules `Foreign.Ptr` and
`Foreign.ForeignPtr`, but C2HS didn't generate any `import`
declarations to make these modules accessible.  This meant that there
would normally be a bit of back and forth when writing C2HS code:
write your bindings, run C2HS, try compiling with GHC, have the
compile fail because of missing imports, add the imports to your
`.chs` file and repeat.  Kind of annoying.

As well as being annoying, the lack of import declaration generation
meant that it was sometimes impossible to make internal changes to the
way that C2HS binds to C functions without breaking existing user
code.  The example that finally drove me to try to fix this was issue
#130 (https://github.com/haskell/c2hs/issues/130) that required a
change that would lead to most C2HS code now needing to import
`unsafePerformIO`.  It didn't seem like a good idea to push a change
like that (that would break more or less all C2HS code out there!)
without fixing the import problem (so that the change for issue #130
could happen transparently to all existing working C2HS code).


#### The solution

The solution I ended up with is pretty simple, but I think it's
robust.  For the example above, C2HS now generates the following
Haskell code:

``` haskell
import qualified Foreign.ForeignPtr as C2HSImp
import qualified Foreign.Ptr as C2HSImp

newtype Foo = Foo (C2HSImp.ForeignPtr (Foo))
withFoo :: Foo -> (C2HSImp.Ptr Foo -> IO b) -> IO b
withFoo (Foo fptr) = C2HSImp.withForeignPtr fptr
```

All library symbols needed to generate Haskell binding code are now
qualified under the name `C2HSImp` and the relevant library modules
are imported qualified as `C2HSImp`.

The end result of this is that you still need to import modules only
for names that you explicitly use (so if you use `alloca` in an input
marshaller, you need to import `Foreign.Marshal.Alloc`).  All external
names that C2HS uses in code that it generates should be imported
automatically.


#### Potential complaints

1. Modules compiled with `-Werror` may now fail because of unused
   import warnings.  This was something I had to deal with for most of
   the C2HS test cases (since they all imported the required library
   modules and they're mostly compiled with `-Werror`), but since the
   community consensus seems to be that `-Werror` shouldn't be used in
   released code, I think it's reasonable to allow the possibility of
   this kind of breakage.

2. It's possible that the code I wrote for deciding where to put the
   extra import declarations isn't quite perfect.  I tried a couple of
   different solutions, but ended up with a hand-made "find the first
   safe place to add imports" function that relies quite heavily on
   the details of C2HS's CHS file parser.  I did try a solution based
   on `haskell-src-exts`, but this didn't work very well, because
   `haskell-src-exts` doesn't support all available GHC extensions and
   I would have needed some mechanism to propagate extension
   information from Cabal files to C2HS to make the parsing work.

These changes have been tested reasonably extensively -- all of the
core C2HS tests pass, and the following packages are known to work
(they're all in the regression suite): abcBridge, alsa-mixer, cuda,
cufft, gnome-keyring, gnuidn, haskell-mpi, hnetcdf, hpuz, hsndfile,
hsshellscript, igraph, libssh2.

I'll be adding more packages to the regression suite, but if there's a
package you're particularly concerned about that's not on this list,
let me know.
