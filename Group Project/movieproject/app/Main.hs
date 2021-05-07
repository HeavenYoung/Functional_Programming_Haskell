{-|
Module      : Main
Description : This is the entry of the application 
Copyright   : (c) 
                  Yang Tian, 
                  Wang Nansu,
                  Zheng Ziqi,
                  Zhang Yizhuo,
License     : GPL-3
Maintainer  : Yang Tian (ml191028@qmul.ac.uk)
Stability   : experimental
Portability : POSIX
-}

module Main where

import HTTPRequest ( loadActorList, loadMovieList )
import DataBaseTool
import GlobalTool
import DataTypes
import ParserTool ()
import Data.Maybe ( fromMaybe )
import Data.Time ()
import Control.Exception ( SomeException, handle )
import Control.Monad ( when )
import qualified Network.HTTP.Conduit as NHC

-- | The main function of this application
main :: IO ()
main = do
  let mainHandler = (print . takeWhile (/= '\n') . show) :: SomeException -> IO ()
  handle mainHandler run

-- | the run fucntion of this application
run :: IO ()
run = do
  -- init and connect to dababase
  conn <- initialiseDB
  
  -- clean invalid out of date in database
  cleanupDatabase conn "2020-08-01"

  let fromDay = "2020-12-10"
  lastMovieDate <- extractLastDateFromDataBase conn

  --  get movie list
  print "movies loading ..."
  let movieHandle = (\e -> return []) :: NHC.HttpException -> IO [Movie]
  movieList <- handle movieHandle (loadMovieList $ fromMaybe fromDay lastMovieDate)
  -- print movieList
  print "movies loaded"

  --  get actor list
  print "actors loading ..."
  let actorHandle = (\e -> return []) :: NHC.HttpException -> IO [Actor]
  actorList <- handle actorHandle (loadActorList movieList)
  print "actors loaded ..."

  --  insert data to database
  print "insert data ..."
  insertMovieIntoDataBase conn movieList
  insertActorIntoDataBase conn actorList
  print "data inserted ..."

  -- get movies from database
  -- movies <- extractMoviesFromDatabase conn
  -- print movies

  -- actors <- extarctAllActorsFromDatabase conn
  -- print actors

  -- input actor name to query movie
  actorName <- enterActorName
  movie <- searchMoviesFromDataBase conn actorName
  case movie of
    Nothing -> do
      disconnectDB conn
      error "Oops.. No item found"
    Just x -> printMovies x
  
  -- movieIndex <- selectAMovie
  -- when (movieIndex > length (fromJust movie)) $ disconnectDB conn
  --   >>= error "Oops.. Input error"
