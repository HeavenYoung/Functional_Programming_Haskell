{-|
Module      : BankSimulator
Description : This is a module to simulate the transfer
Copyright   : (c) Tian Yang
License     : GPL-3
Maintainer  : Yang Tian (tian.yang@hss19.qmul.ac.uk)
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE ScopedTypeVariables #-}

module BankSimulator (
    simulateTransferThread
)
where

import DataTypes
import Say ( sayString )
import Data.Maybe ( fromJust )
import System.Random ( Random(randomRIO) )
import Control.Concurrent ( threadDelay, forkIO )
import Control.Concurrent.STM
    ( atomically, TVar, readTVar, writeTVar, check )
import           Control.Monad (forever, void, when, mapM, forM_)
import qualified Data.Map as Map
import qualified Data.TCache as TC

{- | Simulate transfering from one account to another in a single thread 
  
  step 1, select random accounts ，create thread
  step 2, random amount 10 to 50, and transfer
-}
simulateTransferThread :: Account -> Map.Map Int Account -> (Int, Int) -> IO ()
simulateTransferThread act@(Account id name balance) accounts (rangeStart, rangeEnd) = void $ forkIO $ forever $ do
    interval :: Int <- randomRIO (1, 10) -- 1 ~ 10 seconds
    threadDelay (interval * 1000000)
    targetId :: Int <- randomRIO (rangeStart, rangeEnd)
    let tAct@(Account _ _ tBalance) = fromJust $ targetId `Map.lookup` accounts
    amount :: Int <- randomRIO (10, 50)

    when (id /= targetId) $
        atomically $ do
            curBal <- readTVar balance
            check (curBal >= amount)
            tCurBal <- readTVar tBalance
            writeTVar balance (curBal - amount)
            writeTVar tBalance (tCurBal + amount)
            TC.safeIOToSTM . sayString $ "Account " ++ (show id) ++ " transfered £" ++ (show amount) ++ " to Account " ++ (show targetId)

