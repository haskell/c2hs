-- (c) The FFI task force, [2000..2001]
--
-- Bundles the language-independent FFI library functionality

module Foreign ( 
  module Int,
  module Word,
  module Ptr,
  module ForeignPtr,
  module StablePtr,
  module Storable,
  module MarshalAlloc,
  module MarshalArray,
  module MarshalError,
  module MarshalUtils
) where

import Int
import Word
import Ptr
import ForeignPtr
import StablePtr
import Storable
import MarshalAlloc
import MarshalArray
import MarshalError
import MarshalUtils
