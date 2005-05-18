/* Auxiliary C code for Ghttp.
 *
 * Copyright (c) 1999 Manuel M. T. Chakravarty
 *
 * This is required due to the inability of GHC's FFI to pass structures from C
 * to Haskell.
 */

#ifndef __GHTTPHS_H__
#define __GHTTPHS_H__


#include <ghttp.h>

/* returns a reference to a newly allocated memory area holding the result of 
 * the corresponding vanilla `libghttp' function
 */
ghttp_current_status *ghttpHS_get_status (ghttp_request *a_request);


#endif /* __GHTTPHS_H__ */
