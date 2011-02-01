{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}

import C2HS
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable


#include "test.h"

-- getNumber = {#call unsafe getNumber as _getNumber#}
-- {# fun unsafe test as test {} -> `Int' peekIntConv*  #}
{# fun unsafe test as test {} -> `Int' #}

main = do
     n <- test
     putStrLn $ "test () yields " ++ show n