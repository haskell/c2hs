/* Auxiliary C code for Ghttp.
 *
 * Copyright (c) 1999 Manuel M. T. Chakravarty
 *
 * This is required due to the inability of GHC's FFI to pass structures from C
 * to Haskell.
 */

#include "ghttpHS.h"

ghttp_current_status *ghttpHS_get_status (ghttp_request *a_request)
{
  ghttp_current_status *status;

  status = (ghttp_current_status *) malloc (sizeof (ghttp_current_status));
  if (!status) {
    printf ("Ghttp: ghttpHS_get_status: Out of memory!");
    exit (1);
  }
  *status = ghttp_get_status (a_request);
  return status;
}
