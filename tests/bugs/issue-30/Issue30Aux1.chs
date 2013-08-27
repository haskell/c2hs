module Issue30Aux1 where

import Foreign
import Foreign.C

#include "issue30aux1.h"

{#fun foo1 {`Int'} -> `Int'#}
