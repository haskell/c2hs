module Issue103A where

import Foreign.C.Types

#include "issue103.h"

{#enum test_enum as TestEnum {underscoreToCase} #}
