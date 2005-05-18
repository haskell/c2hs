-- (c) The FFI task force, 2000
--
-- C-specific Marshalling support: Handling of C "errno" error codes

module CError (

  -- Haskell representation for "errno" values
  --
  Errno(..),		-- instance: Eq
  eOK, e2BIG, eACCES, eADDRINUSE, eADDRNOTAVAIL, eADV, eAFNOSUPPORT, eAGAIN, 
  eALREADY, eBADF, eBADMSG, eBADRPC, eBUSY, eCHILD, eCOMM, eCONNABORTED, 
  eCONNREFUSED, eCONNRESET, eDEADLK, eDESTADDRREQ, eDIRTY, eDOM, eDQUOT, 
  eEXIST, eFAULT, eFBIG, eFTYPE, eHOSTDOWN, eHOSTUNREACH, eIDRM, eILSEQ, 
  eINPROGRESS, eINTR, eINVAL, eIO, eISCONN, eISDIR, eLOOP, eMFILE, eMLINK, 
  eMSGSIZE, eMULTIHOP, eNAMETOOLONG, eNETDOWN, eNETRESET, eNETUNREACH, 
  eNFILE, eNOBUFS, eNODATA, eNODEV, eNOENT, eNOEXEC, eNOLCK, eNOLINK, 
  eNOMEM, eNOMSG, eNONET, eNOPROTOOPT, eNOSPC, eNOSR, eNOSTR, eNOSYS, 
  eNOTBLK, eNOTCONN, eNOTDIR, eNOTEMPTY, eNOTSOCK, eNOTTY, eNXIO, 
  eOPNOTSUPP, ePERM, ePFNOSUPPORT, ePIPE, ePROCLIM, ePROCUNAVAIL, 
  ePROGMISMATCH, ePROGUNAVAIL, ePROTO, ePROTONOSUPPORT, ePROTOTYPE, 
  eRANGE, eREMCHG, eREMOTE, eROFS, eRPCMISMATCH, eRREMOTE, eSHUTDOWN, 
  eSOCKTNOSUPPORT, eSPIPE, eSRCH, eSRMNT, eSTALE, eTIME, eTIMEDOUT, 
  eTOOMANYREFS, eTXTBSY, eUSERS, eWOULDBLOCK, eXDEV,
                        -- :: Errno
  isValidErrno,		-- :: Errno -> Bool

  -- access to the current thread's "errno" value
  --
  getErrno,             -- :: IO Errno
  resetErrno,           -- :: IO ()

  -- conversion of an "errno" value into IO error
  --
  errnoToIOError,       -- :: String       -- location
                        -- -> Errno        -- errno
                        -- -> Maybe Handle -- handle
                        -- -> Maybe String -- filename
                        -- -> IOError

  -- throw current "errno" value
  --
  throwErrno,           -- ::                String               -> IO a

  -- guards for IO operations that may fail
  --
  throwErrnoIf,         -- :: (a -> Bool) -> String -> IO a       -> IO a
  throwErrnoIf_,        -- :: (a -> Bool) -> String -> IO a       -> IO ()
  throwErrnoIfRetry,    -- :: (a -> Bool) -> String -> IO a       -> IO a
  throwErrnoIfRetry_,   -- :: (a -> Bool) -> String -> IO a       -> IO ()
  throwErrnoIfMinus1,   -- :: Num a 
			-- =>                String -> IO a       -> IO a
  throwErrnoIfMinus1_,  -- :: Num a 
			-- =>                String -> IO a       -> IO ()
  throwErrnoIfMinus1Retry,  
			-- :: Num a 
			-- =>                String -> IO a       -> IO a
  throwErrnoIfMinus1Retry_,  
			-- :: Num a 
			-- =>                String -> IO a       -> IO ()
  throwErrnoIfNull,	-- ::                String -> IO (Ptr a) -> IO (Ptr a)
  throwErrnoIfNullRetry -- ::                String -> IO (Ptr a) -> IO (Ptr a)
) where


-- regular imports
-- ---------------

import Monad        (liftM)

import Ptr          (Ptr, nullPtr)
import NewStorable  (Storable(..))
import CTypes       (CInt)
import MarshalError (void)

import IO           (IOError, Handle, ioError)


-- "errno" type
-- ------------

-- import of address of C-level "errno" variable
--
foreign label "errno" _errno :: Ptr CInt

-- Haskell representation for "errno" values
--
newtype Errno = Errno CInt

instance Eq Errno where
  errno1@(Errno no1) == errno2@(Errno no2) 
    | isValidErrno errno1 && isValidErrno errno2 = no1 == no2
    | otherwise					 = False

-- common "errno" symbols
--
eOK, e2BIG, eACCES, eADDRINUSE, eADDRNOTAVAIL, eADV, eAFNOSUPPORT, eAGAIN, 
  eALREADY, eBADF, eBADMSG, eBADRPC, eBUSY, eCHILD, eCOMM, eCONNABORTED, 
  eCONNREFUSED, eCONNRESET, eDEADLK, eDESTADDRREQ, eDIRTY, eDOM, eDQUOT, 
  eEXIST, eFAULT, eFBIG, eFTYPE, eHOSTDOWN, eHOSTUNREACH, eIDRM, eILSEQ, 
  eINPROGRESS, eINTR, eINVAL, eIO, eISCONN, eISDIR, eLOOP, eMFILE, eMLINK, 
  eMSGSIZE, eMULTIHOP, eNAMETOOLONG, eNETDOWN, eNETRESET, eNETUNREACH, 
  eNFILE, eNOBUFS, eNODATA, eNODEV, eNOENT, eNOEXEC, eNOLCK, eNOLINK, 
  eNOMEM, eNOMSG, eNONET, eNOPROTOOPT, eNOSPC, eNOSR, eNOSTR, eNOSYS, 
  eNOTBLK, eNOTCONN, eNOTDIR, eNOTEMPTY, eNOTSOCK, eNOTTY, eNXIO, 
  eOPNOTSUPP, ePERM, ePFNOSUPPORT, ePIPE, ePROCLIM, ePROCUNAVAIL, 
  ePROGMISMATCH, ePROGUNAVAIL, ePROTO, ePROTONOSUPPORT, ePROTOTYPE, 
  eRANGE, eREMCHG, eREMOTE, eROFS, eRPCMISMATCH, eRREMOTE, eSHUTDOWN, 
  eSOCKTNOSUPPORT, eSPIPE, eSRCH, eSRMNT, eSTALE, eTIME, eTIMEDOUT, 
  eTOOMANYREFS, eTXTBSY, eUSERS, eWOULDBLOCK, eXDEV		       :: Errno
--
-- the @CCONST_XXX@ identifiers have to replaced by the appropriate
-- system-dependent values
--
eOK             = Errno 0
e2BIG           = Errno (@CCONST_E2BIG@)
eACCES		= Errno (@CCONST_EACCES@)
eADDRINUSE	= Errno (@CCONST_EADDRINUSE@)
eADDRNOTAVAIL	= Errno (@CCONST_EADDRNOTAVAIL@)
eADV		= Errno (@CCONST_EADV@)
eAFNOSUPPORT	= Errno (@CCONST_EAFNOSUPPORT@)
eAGAIN		= Errno (@CCONST_EAGAIN@)
eALREADY	= Errno (@CCONST_EALREADY@)
eBADF		= Errno (@CCONST_EBADF@)
eBADMSG		= Errno (@CCONST_EBADMSG@)
eBADRPC		= Errno (@CCONST_EBADRPC@)
eBUSY		= Errno (@CCONST_EBUSY@)
eCHILD		= Errno (@CCONST_ECHILD@)
eCOMM		= Errno (@CCONST_ECOMM@)
eCONNABORTED	= Errno (@CCONST_ECONNABORTED@)
eCONNREFUSED	= Errno (@CCONST_ECONNREFUSED@)
eCONNRESET	= Errno (@CCONST_ECONNRESET@)
eDEADLK		= Errno (@CCONST_EDEADLK@)
eDESTADDRREQ	= Errno (@CCONST_EDESTADDRREQ@)
eDIRTY		= Errno (@CCONST_EDIRTY@)
eDOM		= Errno (@CCONST_EDOM@)
eDQUOT		= Errno (@CCONST_EDQUOT@)
eEXIST		= Errno (@CCONST_EEXIST@)
eFAULT		= Errno (@CCONST_EFAULT@)
eFBIG		= Errno (@CCONST_EFBIG@)
eFTYPE		= Errno (@CCONST_EFTYPE@)
eHOSTDOWN	= Errno (@CCONST_EHOSTDOWN@)
eHOSTUNREACH	= Errno (@CCONST_EHOSTUNREACH@)
eIDRM		= Errno (@CCONST_EIDRM@)
eILSEQ		= Errno (@CCONST_EILSEQ@)
eINPROGRESS	= Errno (@CCONST_EINPROGRESS@)
eINTR		= Errno (@CCONST_EINTR@)
eINVAL		= Errno (@CCONST_EINVAL@)
eIO		= Errno (@CCONST_EIO@)
eISCONN		= Errno (@CCONST_EISCONN@)
eISDIR		= Errno (@CCONST_EISDIR@)
eLOOP		= Errno (@CCONST_ELOOP@)
eMFILE		= Errno (@CCONST_EMFILE@)
eMLINK		= Errno (@CCONST_EMLINK@)
eMSGSIZE	= Errno (@CCONST_EMSGSIZE@)
eMULTIHOP	= Errno (@CCONST_EMULTIHOP@)
eNAMETOOLONG	= Errno (@CCONST_ENAMETOOLONG@)
eNETDOWN	= Errno (@CCONST_ENETDOWN@)
eNETRESET	= Errno (@CCONST_ENETRESET@)
eNETUNREACH	= Errno (@CCONST_ENETUNREACH@)
eNFILE		= Errno (@CCONST_ENFILE@)
eNOBUFS		= Errno (@CCONST_ENOBUFS@)
eNODATA		= Errno (@CCONST_ENODATA@)
eNODEV		= Errno (@CCONST_ENODEV@)
eNOENT		= Errno (@CCONST_ENOENT@)
eNOEXEC		= Errno (@CCONST_ENOEXEC@)
eNOLCK		= Errno (@CCONST_ENOLCK@)
eNOLINK		= Errno (@CCONST_ENOLINK@)
eNOMEM		= Errno (@CCONST_ENOMEM@)
eNOMSG		= Errno (@CCONST_ENOMSG@)
eNONET		= Errno (@CCONST_ENONET@)
eNOPROTOOPT	= Errno (@CCONST_ENOPROTOOPT@)
eNOSPC		= Errno (@CCONST_ENOSPC@)
eNOSR		= Errno (@CCONST_ENOSR@)
eNOSTR		= Errno (@CCONST_ENOSTR@)
eNOSYS		= Errno (@CCONST_ENOSYS@)
eNOTBLK		= Errno (@CCONST_ENOTBLK@)
eNOTCONN	= Errno (@CCONST_ENOTCONN@)
eNOTDIR		= Errno (@CCONST_ENOTDIR@)
eNOTEMPTY	= Errno (@CCONST_ENOTEMPTY@)
eNOTSOCK	= Errno (@CCONST_ENOTSOCK@)
eNOTTY		= Errno (@CCONST_ENOTTY@)
eNXIO		= Errno (@CCONST_ENXIO@)
eOPNOTSUPP	= Errno (@CCONST_EOPNOTSUPP@)
ePERM		= Errno (@CCONST_EPERM@)
ePFNOSUPPORT	= Errno (@CCONST_EPFNOSUPPORT@)
ePIPE		= Errno (@CCONST_EPIPE@)
ePROCLIM	= Errno (@CCONST_EPROCLIM@)
ePROCUNAVAIL	= Errno (@CCONST_EPROCUNAVAIL@)
ePROGMISMATCH	= Errno (@CCONST_EPROGMISMATCH@)
ePROGUNAVAIL	= Errno (@CCONST_EPROGUNAVAIL@)
ePROTO		= Errno (@CCONST_EPROTO@)
ePROTONOSUPPORT = Errno (@CCONST_EPROTONOSUPPORT@)
ePROTOTYPE	= Errno (@CCONST_EPROTOTYPE@)
eRANGE		= Errno (@CCONST_ERANGE@)
eREMCHG		= Errno (@CCONST_EREMCHG@)
eREMOTE		= Errno (@CCONST_EREMOTE@)
eROFS		= Errno (@CCONST_EROFS@)
eRPCMISMATCH	= Errno (@CCONST_ERPCMISMATCH@)
eRREMOTE	= Errno (@CCONST_ERREMOTE@)
eSHUTDOWN	= Errno (@CCONST_ESHUTDOWN@)
eSOCKTNOSUPPORT = Errno (@CCONST_ESOCKTNOSUPPORT@)
eSPIPE		= Errno (@CCONST_ESPIPE@)
eSRCH		= Errno (@CCONST_ESRCH@)
eSRMNT		= Errno (@CCONST_ESRMNT@)
eSTALE		= Errno (@CCONST_ESTALE@)
eTIME		= Errno (@CCONST_ETIME@)
eTIMEDOUT	= Errno (@CCONST_ETIMEDOUT@)
eTOOMANYREFS	= Errno (@CCONST_ETOOMANYREFS@)
eTXTBSY		= Errno (@CCONST_ETXTBSY@)
eUSERS		= Errno (@CCONST_EUSERS@)
eWOULDBLOCK	= Errno (@CCONST_EWOULDBLOCK@)
eXDEV		= Errno (@CCONST_EXDEV@)

-- checks whether the given errno value is supported on the current
-- architecture
--
isValidErrno               :: Errno -> Bool
--
-- the configure script sets all invalid "errno"s to -1
--
isValidErrno (Errno errno)  = errno /= -1


-- access to the current thread's "errno" value
-- --------------------------------------------

-- yield the current thread's "errno" value
--
getErrno :: IO Errno
getErrno  = liftM Errno (peek _errno)


-- set the current thread's "errno" value to 0
--
resetErrno :: IO ()
resetErrno  = poke _errno 0


-- throw current "errno" value
-- ---------------------------

-- the common case: throw an IO error based on a textual description
-- of the error location and the current thread's "errno" value
--
throwErrno     :: String -> IO a
throwErrno loc  =
  do
    errno <- getErrno
    ioError (errnoToIOError loc errno Nothing Nothing)


-- guards for IO operations that may fail
-- --------------------------------------

-- guard an IO operation and throw an "errno" based exception of the result
-- value of the IO operation meets the given predicate
--
throwErrnoIf            :: (a -> Bool) -> String -> IO a -> IO a
throwErrnoIf pred loc f  = 
  do
    res <- f
    if pred res then throwErrno loc else return res

-- as `throwErrnoIf', but discards the result
--
throwErrnoIf_            :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIf_ pred loc f  = void $ throwErrnoIf pred loc f

-- as `throwErrnoIf', but retries interrupted IO operations (ie, those whose
-- flag `EINTR')
--
throwErrnoIfRetry            :: (a -> Bool) -> String -> IO a -> IO a
throwErrnoIfRetry pred loc f  = 
  do
    res <- f
    if pred res
      then do
	err <- getErrno
	if err == eINTR
	  then throwErrnoIfRetry pred loc f
	  else throwErrno loc
      else return res

-- as `throwErrnoIfRetry', but discards the result
--
throwErrnoIfRetry_            :: (a -> Bool) -> String -> IO a -> IO ()
throwErrnoIfRetry_ pred loc f  = void $ throwErrnoIfRetry pred loc f

-- throws "errno" if a result of "-1" is returned
--
throwErrnoIfMinus1 :: Num a => String -> IO a -> IO a
throwErrnoIfMinus1  = throwErrnoIf (== -1)

-- as `throwErrnoIfMinus1', but discards the result
--
throwErrnoIfMinus1_ :: Num a => String -> IO a -> IO ()
throwErrnoIfMinus1_  = throwErrnoIf_ (== -1)

-- throws "errno" if a result of "-1" is returned, but retries in case of an
-- interrupted operation
--
throwErrnoIfMinus1Retry :: Num a => String -> IO a -> IO a
throwErrnoIfMinus1Retry  = throwErrnoIfRetry (== -1)

-- as `throwErrnoIfMinus1', but discards the result
--
throwErrnoIfMinus1Retry_ :: Num a => String -> IO a -> IO ()
throwErrnoIfMinus1Retry_  = throwErrnoIfRetry_ (== -1)

-- throws "errno" if a result of a NULL pointer is returned
--
throwErrnoIfNull :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNull  = throwErrnoIf (== nullPtr)

-- throws "errno" if a result of a NULL pointer is returned, but retries in
-- case of an interrupted operation
--
throwErrnoIfNullRetry :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNullRetry  = throwErrnoIfRetry (== nullPtr)


-- conversion of an "errno" value into IO error
-- --------------------------------------------

-- convert a location string, an "errno" value, an optional handle,
-- and an optional filename into a matching IO error
--
errnoToIOError :: String -> Errno -> Maybe Handle -> Maybe String -> IOError
errnoToIOError loc errno@(Errno no) maybeHdl maybeName =
  userError (loc ++ ": " ++ str ++ maybe "" (": "++) maybeName)
  where
    str
      | errno == eOK              = "no error"
      | errno == e2BIG            = "argument list too long"
      | errno == eACCES		  = "inadequate access permission"
      | errno == eADDRINUSE	  = "address already in use"
      | errno == eADDRNOTAVAIL	  = "address not available"
      | errno == eADV		  = "RFS advertise error"
      | errno == eAFNOSUPPORT	  = "address family not supported by \
				     \protocol family"
      | errno == eAGAIN		  = "insufficient resources"
      | errno == eALREADY	  = "operation already in progress"
      | errno == eBADF		  = "internal error (EBADF)"
      | errno == eBADMSG	  = "next message has wrong type"
      | errno == eBADRPC	  = "invalid RPC request or response"
      | errno == eBUSY		  = "device busy"
      | errno == eCHILD		  = "no child processes"
      | errno == eCOMM		  = "no virtual circuit could be found"
      | errno == eCONNABORTED	  = "aborted connection"
      | errno == eCONNREFUSED	  = "no listener on remote host"
      | errno == eCONNRESET	  = "connection reset by peer"
      | errno == eDEADLK	  = "resource deadlock avoided"
      | errno == eDESTADDRREQ	  = "destination address required"
      | errno == eDIRTY		  = "file system dirty"
      | errno == eDOM		  = "argument too large"
      | errno == eDQUOT		  = "quota exceeded"
      | errno == eEXIST		  = "file already exists"
      | errno == eFAULT		  = "internal error (EFAULT)"
      | errno == eFBIG		  = "file too large"
      | errno == eFTYPE		  = "inappropriate NFS file type or format"
      | errno == eHOSTDOWN	  = "destination host down"
      | errno == eHOSTUNREACH	  = "remote host is unreachable"
      | errno == eIDRM		  = "IPC identifier removed"
      | errno == eILSEQ		  = "invalid wide character"
      | errno == eINPROGRESS	  = "operation now in progress"
      | errno == eINTR		  = "interrupted system call"
      | errno == eINVAL		  = "invalid argument"
      | errno == eIO		  = "unknown I/O fault"
      | errno == eISCONN	  = "socket is already connected"
      | errno == eISDIR		  = "file is a directory"
      | errno == eLOOP		  = "too many symbolic links"
      | errno == eMFILE		  = "process file table full"
      | errno == eMLINK		  = "too many links"
      | errno == eMSGSIZE	  = "message too long"
      | errno == eMULTIHOP	  = "multi-hop RFS request"
      | errno == eNAMETOOLONG	  = "filename too long"
      | errno == eNETDOWN	  = "network is down"
      | errno == eNETRESET	  = "remote host rebooted; connection lost"
      | errno == eNETUNREACH	  = "remote network is unreachable"
      | errno == eNFILE		  = "system file table full"
      | errno == eNOBUFS	  = "no buffer space available"
      | errno == eNODATA	  = "no message on the stream head read queue"
      | errno == eNODEV		  = "no such device"
      | errno == eNOENT		  = "no such file or directory"
      | errno == eNOEXEC	  = "not an executable file"
      | errno == eNOLCK		  = "no file locks available"
      | errno == eNOLINK	  = "RFS link has been severed"
      | errno == eNOMEM		  = "not enough virtual memory"
      | errno == eNOMSG		  = "no message of desired type"
      | errno == eNONET		  = "host is not on a network"
      | errno == eNOPROTOOPT	  = "operation not supported by protocol"
      | errno == eNOSPC		  = "no space left on device"
      | errno == eNOSR		  = "out of stream resources"
      | errno == eNOSTR		  = "not a stream device"
      | errno == eNOSYS		  = "function not implemented"
      | errno == eNOTBLK	  = "not a block device"
      | errno == eNOTCONN	  = "socket is not connected"
      | errno == eNOTDIR	  = "not a directory"
      | errno == eNOTEMPTY	  = "directory not empty"
      | errno == eNOTSOCK	  = "not a socket"
      | errno == eNOTTY		  = "inappropriate ioctl for device"
      | errno == eNXIO		  = "no such device or address"
      | errno == eOPNOTSUPP	  = "operation not supported on socket"
      | errno == ePERM		  = "privileged operation"
      | errno == ePFNOSUPPORT	  = "protocol family not supported"
      | errno == ePIPE		  = "broken pipe"
      | errno == ePROCLIM	  = "too many processes"
      | errno == ePROCUNAVAIL	  = "unimplemented RPC procedure"
      | errno == ePROGMISMATCH	  = "unsupported RPC program version"
      | errno == ePROGUNAVAIL	  = "RPC program unavailable"
      | errno == ePROTO		  = "error in streams protocol"
      | errno == ePROTONOSUPPORT  = "protocol not supported"
      | errno == ePROTOTYPE	  = "wrong protocol for socket"
      | errno == eRANGE		  = "result too large"
      | errno == eREMCHG	  = "remote address changed"
      | errno == eREMOTE	  = "too many levels of remote in path"
      | errno == eROFS		  = "read-only file system"
      | errno == eRPCMISMATCH	  = "RPC version is wrong"
      | errno == eRREMOTE	  = "object is remote"
      | errno == eSHUTDOWN        = "can't send after socket shutdown"
      | errno == eSOCKTNOSUPPORT  = "socket type not supported"
      | errno == eSPIPE		  = "can't seek on a pipe"
      | errno == eSRCH		  = "no such process"
      | errno == eSRMNT		  = "RFS resources still mounted by \
				    \remote host(s)")
      | errno == eSTALE		  = "stale NFS file handle"
      | errno == eTIME		  = "timer expired"
      | errno == eTIMEDOUT	  = "connection timed out"
      | errno == eTOOMANYREFS	  = "too many references; can't splice"
      | errno == eTXTBSY	  = "text file in-use"
      | errno == eUSERS		  = "quota table full"
      | errno == eWOULDBLOCK	  = "operation would block"
      | errno == eXDEV		  = "can't make a cross-device link"
      | otherwise                 = "unexpected error (error code: "
				     ++ show no ++ ")"
