{-|
Module      : Bank
Description : This is a module to define haskell data and type
Copyright   : (c) Tian Yang
License     : GPL-3
Maintainer  : Yang Tian (tian.yang@hss19.qmul.ac.uk)
Stability   : experimental
Portability : POSIX
-}

module DataTypes(
       Account(Account)
) where

import Control.Concurrent ()
import Control.Concurrent.STM ( TVar )
import           Control.Monad (forever, void, when, mapM, forM_)
import qualified Data.Map as Map
import qualified Data.TCache as TC



{- | 

Data & Types

Data for bank account 

Account (id , name, balance)
-}
data Account = Account
    { id      :: Int
    , name    :: String
    , balance :: TVar Int
    }