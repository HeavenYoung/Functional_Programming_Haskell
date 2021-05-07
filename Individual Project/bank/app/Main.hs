{-|
Module      : Bank
Description : This is a main entry of the application
Copyright   : (c) Tian Yang
License     : GPL-3
Maintainer  : Yang Tian (tian.yang@hss19.qmul.ac.uk)
Stability   : experimental
Portability : POSIX
-}

module Main where

import Say ( sayString )
import BankSimulator
import DataTypes ( Account(Account) )
import System.Environment ( getArgs )
import Control.Concurrent ( threadDelay )
import Control.Concurrent.STM ( atomically, newTVarIO, readTVar )
import Control.Monad (forever, void, when, mapM, forM_)
import Control.Concurrent.Async ( async, wait )
import Control.Parallel.Strategies ()
import qualified Data.Map as Map

{- | Main fucntion of the Application, when we put "stack run normal", the application will run with normal mode, 
     and "stack ghc -- -threaded app/Main.hs && ./app/Main parallel +RTS -N2" to run the bank system in two cores
   
    step 1. spawn an account id list and excute the run function
-}
main :: IO ()
main = do
    args <- getArgs
    case args of
      ["normal"] -> do
          -- spawn an account id list and run
          run [1..10]
      ["parallel"] -> do
          async $ run [1..5]
          (async $ run [6..10]) >>= wait
      _ -> syntaxError

{- | syntaxError -}
syntaxError = putStrLn
   "Usage: Bank command [args]\n\
   \\n\
   \normal      Run bank with single core\n\
   \parallel    Run bank in parallel\n"

{- | Run the bank simulator with concrete account id list 
     step 1, creat 10 customers
     step 2, simulate transferring
     step 3. print the current account balance after the each transferring
-}
run :: [Int] -> IO ()
run accountIds = do
    accounts <- mapM (\aid -> do
        balanceVar <- newTVarIO 1000 -- starting with £1000
        return $ (aid, Account aid (show aid) balanceVar)
        ) accountIds
    let accountDict = Map.fromList accounts

    forM_ accounts (\(_, act) -> simulateTransferThread act accountDict (head accountIds, last accountIds))

    void $ forever $ do
        forM_ accounts (\(aid, act@(Account _ _ balance)) -> do
            curBal <- atomically $ readTVar balance
            sayString $ "Balance of Account " ++ (show aid) ++ " is " ++ (show curBal)) -- print the balance of current account
        threadDelay 5000000 -- query every 5 seconds



