module Main where

#include "issue46.h"

{#pointer *oid as Oid foreign newtype#}

{#fun func as ^ {`Int', `Float'} -> +`Oid'#}
