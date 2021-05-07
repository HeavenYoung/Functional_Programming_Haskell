{-|
Module      : DataTypes
Description : This is a module of general tools
Copyright   : (c) 
                  Tian Yang, 
                  Nansu Wang,
                  Ziqi Zheng,
                  Yizhuo Zhang,
License     : GPL-3
Stability   : experimental
Portability : POSIX
-}

module GlobalTool
    ( enterActorName,
      selectAMovie,
      printMovies,
      getCurrentTimeString
    ) where

import DataTypes
import Data.Time

-- | ask user to type the actor name to search movies
enterActorName :: IO String
enterActorName = do
  putStrLn "Please enter the actor name you want to search for: "
  getLine

-- | ask user to select a movie
selectAMovie :: IO Int
selectAMovie = do
  putStrLn "Please select a movie from the List: "
  readLn

-- | get current time and return a string
getCurrentTimeString :: IO String
getCurrentTimeString = do
  date <- getCurrentTime
  print (utctDay date)
  return . show $ utctDay date

-- | prints a given list of movies
printMovies :: [Movie] -> IO()
printMovies x = do
  putStrLn "The given actor plays in the following movies"
  printMoviesHelper x 1
    where
      printMoviesHelper :: [Movie] -> Int -> IO()
      printMoviesHelper [] _ = return ()
      printMoviesHelper (x:xs) i = do
        putStrLn ("(" ++ show i ++ ") " ++ show x)
        printMoviesHelper xs (i + 1)
