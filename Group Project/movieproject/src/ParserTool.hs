{-|
Module      : DataTypes
Description : This is a module to parse data
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

{-# LANGUAGE OverloadedStrings #-}

module ParserTool
    (parseMovies, 
     parseActors, 
     parsePages
    ) where

import DataTypes ( Actor(..), Movie(..) )
import Data.Aeson
    ( decode, (.:), withObject, FromJSON(parseJSON), Value(Object) )
import Data.Aeson.Types ( Parser, parseMaybe )
import Control.Monad ( MonadPlus(mzero) )
import qualified Data.ByteString.Lazy as LB
import Data.Maybe ( fromMaybe )

data Results = Results {id :: Int, title :: String, releaseDate :: String}
  deriving (Show)

data MovieFromJSON = MovieFromJSON {results :: [Results], pages :: Int}
  deriving (Show)

instance FromJSON Results where
   parseJSON (Object v) = Results <$> v .: "id" <*> v .: "title" <*> v .: "release_date"
   parseJSON _ = mzero

instance FromJSON MovieFromJSON where
   parseJSON (Object v) = MovieFromJSON <$> v .: "results" <*> v .: "total_pages"
   parseJSON _ = mzero

parsePages :: LB.ByteString -> Int
parsePages b = fromMaybe 1 (parseMaybe pageParser =<< decode b)

pageParser :: Value -> Parser Int
pageParser = withObject "pageParser" $ \o -> o.: "total_pages"

instance FromJSON Movie where
   parseJSON (Object o) = Movie <$> o .: "id" <*> o .: "title" <*> o .: "release_date"
   parseJSON _ = mzero

-- parse movie data and return a lsit of movie
parseMovies :: LB.ByteString -> [Movie]
parseMovies b = fromMaybe [] (parseMaybe movieParser =<< decode b)

movieParser :: Value -> Parser [Movie]
movieParser = withObject "movieParser" $ \o -> o.: "results"

data TmpActor = TmpActor Int String

instance FromJSON TmpActor where
    parseJSON (Object o) = TmpActor <$> o .: "id" <*> o .: "name"
    parseJSON _ = mzero

-- parse actor data and return an list of actor
parseActors :: LB.ByteString -> Movie -> [Actor]
parseActors cn movie = map (converteTmp movie) (fromMaybe [] (parseMaybe actorParser =<< decode cn))
  where
    converteTmp :: Movie -> TmpActor -> Actor
    converteTmp movie (TmpActor aId name) = Actor aId name [movie]

actorParser :: Value -> Parser [TmpActor]
actorParser = withObject "actorParser" $ \o -> o.: "cast"
