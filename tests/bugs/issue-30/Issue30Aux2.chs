module Issue30Aux2 where

import Foreign
import Foreign.C

#include "issue30aux2.h"

{#fun foo2 {`Int'} -> `Int'#}
