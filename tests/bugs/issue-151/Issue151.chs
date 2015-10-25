module Issue151 where

{# context lib = "gdal" prefix = "CPL" #}

#include "issue151.h"

{#pointer ErrorHandler#}

main :: IO ()
main = print "OK"
