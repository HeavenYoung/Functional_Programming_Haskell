{-|
Module      : DataTypes
Description : This is a module to do all Network request
Copyright   : (c) 
                  Tian Yang, 
                  Nansu Wang,
                  Ziqi Zheng,
                  Yizhuo Zhang,
License     : GPL-3
Maintainer  : Yang Tian (ml191028@qmul.ac.uk)
Stability   : experimental
Portability : POSIX
-}

module HTTPRequest
    ( loadMovieList,
      loadActorList
    ) where

import DataTypes ( Actor(..), Movie(..) )
import ParserTool ( parseActors, parseMovies, parsePages )
import GlobalTool ()
import Data.List ()
import Control.Monad ( (<=<) )
import qualified Network.HTTP.Conduit as NHC
import qualified Data.ByteString.Lazy as LB
import Control.Exception ( handle )
import Data.Time
-- import Formatting

-- | get all movie from a given date
loadMovieList :: String -> IO [Movie]
loadMovieList fromDate = do

  let movieHandle = (\e -> return LB.empty) :: NHC.HttpException -> IO LB.ByteString
  
  -- get total_pages
  totolPages <- fmap parsePages (handle movieHandle (NHC.simpleHttp =<< getMovieRequestURL fromDate 1))
  -- print totolPages
  requestList <- mapM (NHC.simpleHttp <=< getMovieRequestURL fromDate) [1..totolPages]

  -- concatenate all response
  return $ concatMap parseMovies requestList

-- | produces the URL for the API call from a given date and a page number. before using the tMDB , we need to register a key
getMovieRequestURL :: String -> Int -> IO String
getMovieRequestURL fromDate i = do

  -- date <- getCurrentTime
  -- print (utctDay date)
  
  return $ concat ["https://api.themoviedb.org/3/discover/movie?api_key=",
                   "75bdf334ec04a7656cf07bd238d48036&language=en-US&sort_by=",
                   "popularity.desc&include_adult=false&include_video=false&page=",
                   show i, "&region=US&primary_release_date.gte=", fromDate,
                   "&primary_release_date.lte=","2020-12-31"]

-- | load actors
loadActorList :: [Movie] -> IO [Actor]
loadActorList movies = do
  actors <- mapM getActorResponse movies
  return $ filterActors actors

-- | get actor response 
getActorResponse :: Movie -> IO [Actor]
getActorResponse m@(Movie movieId _ _) = do
  let actorHandle = (\e -> return LB.empty) :: NHC.HttpException -> IO LB.ByteString
  actorResponse <- handle actorHandle (NHC.simpleHttp (getActorRequestUrl movieId))
  return $ parseActors actorResponse m

-- | concatenate actor url
getActorRequestUrl :: Int -> String
getActorRequestUrl aId = concat [ "https://api.themoviedb.org/3/movie/", show aId,
                          "/credits?api_key=75bdf334ec04a7656cf07bd238d48036" ]

-- | remove duplicted actors
filterActors :: [[Actor]] -> [Actor]
filterActors x = removeDups $ concat x
  where
    removeDups :: [Actor] -> [Actor]
    removeDups [] = []
    removeDups (x:xs) = let dups = getDups x (x:xs) in concatA (fst dups) : removeDups (snd dups)

    getDups :: Actor -> [Actor] -> ([Actor], [Actor])
    getDups _ []                                       = ([], [])
    getDups a1@(Actor _ n1 _) (a2@(Actor _ n2 _) : xs) = if n1 == n2
                                                          then (a2 : fst (getDups a1 xs), snd $ getDups a1 xs)
                                                          else (fst $ getDups a1 xs, a2 : snd (getDups a1 xs))
    concatA :: [Actor] -> Actor
    concatA [a]                                         = a
    concatA (Actor aId1 n1 m1 : Actor aId2 n2 m2 : xs ) = concatA (Actor aId1 n1 (m1 ++ m2) : xs)
