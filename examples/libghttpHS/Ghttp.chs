--  GhttpHS: Haskell binding to the Gnome HTTP library		  -*-haskell-*-
--
--  Author : Manuel M. T. Chakravarty
--  Created: 5 August 99
--
--  Copyright (c) [1999..2000] Manuel M. T. Chakravarty
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  The C library `libghttp' provides a set of common http functions needed at 
--  the client and the server end of an http connection.  The Haskell binding
--  is generated with the help of the C->Haskell tool - always modify the
--  original .chs file, _not_ the generated .hs file.
--
--  This library is fully compliant with HTTP 1.1 as defined in the draft 5
--  update of RFC 2068.  
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98 & C->HS binding hooks (v0.7.5)
--
--  ** Stylistic warning: In the definition of `CurrentStatus', the field
--       labels do not contain the name of the data type to which they belong.
--       This is _not_ good practice in larger interfaces, because in Haskell
--       such field labels pollute the global name space.
--
--- TODO ----------------------------------------------------------------------
--
--  * When and by whom is the memory area passed to `ghttp_set_body' be freed; 
--    how about the string returned from `ghttp_get_body'?
--
--  * Conversion of `time_t' misses for `parseDate'.
--

module Ghttp (Request, URI, Type(..), SyncMode(..), Status(..), Proc(..),
	      CurrentStatus(..),
	      requestNew, requestDestroy, uriValidate, setURI, setProxy,
	      setType, setBody, setSync, prepare, setChunksize, setHeader,
	      process, getStatus, getHeader, close, clean, getSocket, getBody, 
	      getError, {-parseDate,-} setAuthinfo, setProxyAuthinfo)
where

-- C->HS marshalling library
--
import C2HS

import Monad  (liftM, when)
import IOExts (unsafePerformIO)


{#context lib="libghttp" prefix="ghttp"#}


-- data structures
-- ---------------

-- abstract handle for a http request object (EXPORTED ABSTRACTLY)
--
newtype Request = Request Addr

-- Uniform Resource Indicators (EXPORTED)
--
type URI = String

-- body type (EXPORTED)
--
{#enum ghttp_type as Type {underscoreToCase}#}

-- synchronous/asynchronous mode (EXPORTED)
--
{#enum sync_mode as SyncMode {underscoreToCase}#}

-- request status (EXPORTED)
--
{#enum status as Status {underscoreToCase}#}

-- describes the activity of a request (EXPORTED)
--
{#enum proc as Proc {underscoreToCase}#}

-- status descriptor (EXPORTED)
--
data CurrentStatus = CurrentStatus {
		       proc       :: Proc,    -- What's it doing?
		       bytesRead  :: Int,     -- How many bytes have been read?
		       bytesTotal :: Int      -- How many bytes total?
		     }

-- error types
--
invalidURI, illegalRequest :: String
invalidURI     = "Ghttp: The Uniform Resource Indicator is invalid."
illegalRequest = "Ghttp: The request is illegal or unsupported."


-- functions
-- ---------

-- create a new request object (EXPORTED)
--
requestNew :: IO Request
requestNew  = liftM Request {#call unsafe request_new#}

-- delete a current request object (EXPORTED)
--
requestDestroy                :: Request -> IO ()
requestDestroy (Request reqa)  = {#call unsafe request_destroy#} reqa

-- validate a uri (EXPORTED)
--
uriValidate     :: URI -> Bool
uriValidate uri  = 
  let res = unsafePerformIO $
	     {#call unsafe uri_validate#} `marsh1_` (stdAddr uri :> free)
  in
  res == -1

-- set a uri in a request (EXPORTED)
--
-- * raise an exception if the URI is not valid
--
setURI                    :: Request -> URI -> IO ()
setURI (Request reqa) uri  = 
  {#call unsafe set_uri#} reqa `marsh1_` (stdAddr uri :> free)
  `ifNegRaise_` invalidURI

-- set a proxy for a request (EXPORTED)
--
-- * raise an exception if the request is not valid
--
setProxy                    :: Request -> URI -> IO ()
setProxy (Request reqa) uri  = 
  {#call unsafe set_proxy#} reqa `marsh1_` (stdAddr uri :> free)
  `ifNegRaise_` illegalRequest

-- set a request type (EXPORTED)
--
-- * raise an exception if the request is not valid
--
setType                      :: Request -> Type -> IO ()
setType (Request reqa) rtype  = 
  {#call unsafe set_type#} reqa (cFromEnum rtype)
  `ifNegRaise_` illegalRequest

-- set the body (EXPORTED)
--
-- * raise an exception if the request is not valid
--
setBody                     :: Request -> String -> IO ()
setBody (Request reqa) body  =
  do
    (box, len) <- listToAddrWithLen body
    {#call unsafe set_body#} reqa box (cFromInt len)
      `ifNegRaise_` illegalRequest

-- set whether or not you want to use sync or async mode (EXPORTED)
--
-- * raise an exception if the request is not valid
--
setSync                      :: Request -> SyncMode -> IO ()
setSync (Request reqa) smode  =
  {#call unsafe set_sync#} reqa (cFromEnum smode)
  `ifNegRaise_` illegalRequest

-- Prepare a request; call this before trying to process a request or if you
-- change the uri (EXPORTED)
--
-- * raise an exception if the request is not valid
--
prepare                :: Request -> IO ()
prepare (Request reqa)  =
  {#call unsafe prepare#} reqa
  `ifNegRaise_` illegalRequest

-- set the chunk size; you might want to do this to optimize for different
-- connection speeds (EXPORTED)
--
setChunksize			 :: Request -> Int -> IO ()
setChunksize (Request reqa) size  =
  {#call unsafe set_chunksize#} reqa (cFromInt size)

-- set a random request header (EXPORTED)
--
setHeader                        :: Request -> String -> String -> IO ()
setHeader (Request reqa) hdr val  =
  {#call unsafe set_header#} reqa
    `marsh2_` (stdAddr hdr :> free)
    $         (stdAddr val :> free)

-- process a request (EXPORTED)
--
process                :: Request -> IO Status
process (Request reqa)  = liftM cToEnum $ {#call unsafe process#} reqa

-- get the status of a request (EXPORTED)
--
getStatus                :: Request -> IO CurrentStatus
getStatus (Request reqa)  =
  {#call unsafe ghttpHS_get_status #} reqa >>= cFromCurrentStatus

-- get the value of a random response header (EXPORTED)
--
getHeader                    :: Request -> String -> IO String
getHeader (Request reqa) hdr  =
  {#call unsafe get_header#} reqa `marsh1_` (stdAddr hdr :> free)
  >>= addrStd

-- abort a currently running request (EXPORTED)
--
-- * raise an exception if the request is not valid
--
close                :: Request -> IO ()
close (Request reqa)  = {#call unsafe close#} reqa
			`ifNegRaise_` illegalRequest

-- clean a request (EXPORTED)
--
clean                :: Request -> IO ()
clean (Request reqa)  = {#call unsafe clean#} reqa

-- get the socket associated with a particular connection (EXPORTED)
--
-- * raise an exception if the request is not valid
--
getSocket                :: Request -> IO Int
getSocket (Request reqa)  = {#call unsafe get_socket#} reqa
			    `ifNegRaise` illegalRequest

-- get the return entity body (EXPORTED)
--
-- * this includes getting the length with `ghttp_get_body_len', as the string 
--   is not necessarily \0 terminated
--
getBody                :: Request -> IO String
getBody (Request reqa)  = 
  do
    bodyAddr <- {#call unsafe get_body#} reqa
		`ifNullRaise` illegalRequest
    bodyLen  <- {#call unsafe get_body_len#} reqa
    addrWithLenToList bodyAddr (cToInt bodyLen)

-- get an error message for a request that has failed (EXPORTED)
--
getError :: Request -> IO String
getError (Request reqa)  = 
  {#call unsafe get_error#} reqa >>= addrStd

-- parse a date string that is one of the standard date formats (EXPORTED)
--
{-parseDate     :: String -> CalendarTime
parseDate str  = 
  do
    time_t <- {#call unsafe parse_date#} `fromString` str
    time <- toCalendarTime
 -}

-- return the status code (EXPORTED)
--
statusCode                :: Request -> IO Int
statusCode (Request reqa)  = 
  liftM cToInt $ {#call unsafe status_code#} reqa

-- return the reason phrase (EXPORTED)
--
-- * raise an exception if the request is not valid
--
reasonPhrase                :: Request -> IO String
reasonPhrase (Request reqa)  = 
  ({#call unsafe reason_phrase#} reqa
   `ifNullRaise` illegalRequest
  ) >>= addrStd

-- set your username/password pair (EXPORTED)
--
-- * raise an exception if the request is not valid
--
setAuthinfo                          :: Request -> String -> String -> IO ()
setAuthinfo (Request reqa) user pass  =
  ({#call unsafe set_authinfo#} reqa 
     `marsh2_` (stdAddr user :> free)
     $         (stdAddr pass :> free)
  )
  `ifNegRaise_` illegalRequest

-- set your username/password pair for proxy (EXPORTED)
--
-- * raise an exception if the request is not valid
--
setProxyAuthinfo  :: Request -> String -> String -> IO ()
setProxyAuthinfo (Request reqa) user pass =
  ({#call unsafe set_proxy_authinfo#} reqa 
     `marsh2_` (stdAddr user :> free)
     $         (stdAddr pass :> free)
  )
  `ifNegRaise_` illegalRequest


-- auxiliary marshalling function
-- -------------------------------

-- marshal the elements of a `ghttp_current_status' struct to Haskell land
--
-- * frees the C struct
--
cFromCurrentStatus       :: Addr -> IO CurrentStatus
cFromCurrentStatus csPtr  = 
  do
    proc <- liftM cToEnum$ {#get current_status.proc#}        csPtr
    read <- liftM cToInt $ {#get current_status.bytes_read#}  csPtr
    total<- liftM cToInt $ {#get current_status.bytes_total#} csPtr
    free csPtr
    return $ CurrentStatus {
	       proc       = proc,
	       bytesRead  = read,
	       bytesTotal = total
	     }
