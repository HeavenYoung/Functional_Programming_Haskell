{-|
Module      : DataTypes
Description : This is a module containning self-defined haskell data
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

module DataTypes (
      Movie(Movie),
      Actor(Actor)
      ) where

-- | Movie, the first param is movie id, the second is movie title, and the last is release date
data Movie = Movie { movieId :: Int, title :: String, releaseDate :: String }
  deriving (Eq)

instance Show Movie where
  show (Movie _ title _) = title

-- | actor, the first param is actor id , the second is actor name, and the last is an arrary contains all movies the actor plays
data Actor = Actor { actorId :: Int, actorName :: String, movie :: [Movie]}
  deriving (Eq, Show)
