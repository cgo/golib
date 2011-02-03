{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

import C2HS
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable


#include "test.h"

{# context lib="golib" #}

-- getNumber = {#call unsafe getNumber as _getNumber#}
-- {# fun unsafe test as test {} -> `Int' peekIntConv*  #}
{# fun unsafe test as test {} -> `Int' #}

{# pointer *golib_matrix as Matrix foreign newtype #}
{# fun unsafe golib_matrix_new as matrixNew {} -> `Ptr Matrix' id #}
-- {# fun unsafe golib_matrix_new as matrixNew {} -> `Matrix' id #}

main = do
     n <- test
     putStrLn $ "test () yields " ++ show n