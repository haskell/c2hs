-- -*-haskell-*-

module Cpp
where

#if (VERSION == 1)

-- this does not match the C definition
--
foo :: Int -> Int
foo = {#call fooC#}

#else

-- this does
--
foo :: Int -> Int -> Int
foo = {#call fooC#}

#end


-- C code

#c

#define VERSION 2

int fooC (int, int);

#endc

