{-|
Module      : DataBaseTool
Description : This is a module to excute all database operation like insert, delete and select 
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

module DataBaseTool
    ( 
      initialiseDB, 
      dropTablesFromDatabase,
      cleanupDatabase,
      disconnectDB,
      insertMovieIntoDataBase,
      insertActorIntoDataBase,
      searchMoviesFromDataBase,
      extarctAllMoviesFromDatabase,
      extarctAllActorsFromDatabase,
      extractLastDateFromDataBase
    ) where

import DataTypes ( Actor(..), Movie(..) )
import Database.HDBC
    ( fromSql,
      toSql,
      quickQuery',
      SqlValue,
      Statement(executeMany),
      IConnection(commit, disconnect, prepare, run) )
import Database.HDBC.Sqlite3 ( connectSqlite3, Connection )

-- | initiliase database
initialiseDB :: IO Connection  
initialiseDB = 
  do
   conn <- connectSqlite3 "database.sqlite"
   run conn ("CREATE TABLE IF NOT EXISTS movies (movieId INT PRIMARY KEY ON " ++
             "CONFLICT IGNORE, name TEXT NOT NULL, release TEXT NOT NULL)") []
   run conn ("CREATE TABLE IF NOT EXISTS actors (actorId INT PRIMARY KEY ON " ++
             "CONFLICT IGNORE, name TEXT NOT NULL)") []
   run conn ("CREATE TABLE IF NOT EXISTS plays (actorId INT NOT NULL, movieId " ++
             "INT NOT NULL, PRIMARY KEY (actorId, movieId) ON CONFLICT IGNORE, " ++
             "FOREIGN KEY (movieId) REFERENCES movies(movieId), FOREIGN KEY " ++
             "(actorId) REFERENCES actors(actorId))") []
   commit conn
   return conn

-- | insert movies into the movies table
insertMovieIntoDataBase :: Connection -> [Movie] -> IO ()
insertMovieIntoDataBase conn movie = do
    let f (Movie movieId name release) = [toSql movieId, 
                                          toSql name, 
                                          toSql release]
    let args = map f movie
    stmt <- prepare conn "INSERT INTO movies VALUES (?, ?, ?)"
    executeMany stmt args
    commit conn

-- | insert actors into actor table
insertActorIntoDataBase :: Connection -> [Actor] -> IO ()
insertActorIntoDataBase conn actors = do
    let f (Actor actorId name _) = [toSql actorId, 
                                    toSql name]
    let actorArgs = map f actors
    stmt <- prepare conn "INSERT INTO actors VALUES (?, ?)"
    executeMany stmt actorArgs
    let f (Actor actorId n []) = []
        f (Actor actorId n (Movie movieId _ _ : xs)) = [toSql actorId, 
                                                        toSql movieId]
                                                       : f (Actor actorId n xs)
    let playsArgs = concatMap f actors
    stmt <- prepare conn "INSERT INTO plays VALUES (?, ?)"
    executeMany stmt playsArgs
    commit conn

-- | get all movies acted by given actor
searchMoviesFromDataBase :: Connection -> String -> IO (Maybe [Movie])
searchMoviesFromDataBase conn name = do
  result <- quickQuery' conn ("SELECT movies.* FROM movies, actors, plays " ++
                              "WHERE actors.name == ? " ++
                              "AND actors.actorId = plays.actorId " ++
                              "AND  plays.movieId = movies.movieId") [toSql name]
  case result of
    [] -> return Nothing
    x  -> return (Just $ convertFromSql x)
  where
    convertFromSql :: [[SqlValue]] -> [Movie]
    convertFromSql = map (\x -> Movie (fromSql $ head x) (fromSql $ x !! 1)
                         (fromSql $ x !! 2))

-- | disconnect the database
disconnectDB :: Connection -> IO ()
disconnectDB = disconnect

-- | drop all tables 
dropTablesFromDatabase :: Connection -> IO()
dropTablesFromDatabase conn = do
   run conn "DROP TABLE IF EXISTS movies" []
   run conn "DROP TABLE IF EXISTS actors" []
   run conn "DROP TABLE IF EXISTS plays" []
   commit conn

-- | remove all data before a given day
cleanupDatabase :: Connection -> String -> IO ()
cleanupDatabase conn date = do
   result <- quickQuery' conn ("SELECT movies.movieId FROM movies WHERE " ++
              "movies.release < ?") [toSql date]
   stmt <- prepare conn "DELETE FROM movies WHERE movies.movieId = ?"
   executeMany stmt result
   stmt <- prepare conn "DELETE FROM plays WHERE plays.movieId = ?"
   executeMany stmt result
   run conn ("DELETE FROM actors WHERE NOT EXISTS (SELECT * FROM plays WHERE " ++
             "actors.actorId == plays.actorId)") []
   commit conn

-- | select all movies from movies table
extarctAllMoviesFromDatabase :: Connection -> IO [Movie]
extarctAllMoviesFromDatabase conn = do
  result <- quickQuery' conn "SELECT * FROM movies" []
  return $ map (\x -> Movie (fromSql $ head x) (fromSql $ x !! 1)
                (fromSql $ x !! 2)) result

-- | select all actors from actors table 
extarctAllActorsFromDatabase :: Connection -> IO [Actor]
extarctAllActorsFromDatabase conn = do
  result <- quickQuery' conn "SELECT * FROM actors" []
  let ids = map head result
  result2 <- mapM (\x -> quickQuery' conn ("SELECT movies.* FROM movies, plays " ++
              "WHERE ? = plays.actorId AND plays.movieId = movies.movieId") [x]) ids
  let finalResult = zip result result2
  return $ map (\x -> Actor (fromSql $ head $ fst x) (fromSql $ fst x !! 1)
    (map parseMovie (snd x))) finalResult
    where
      parseMovie :: [SqlValue] -> Movie
      parseMovie x = Movie (fromSql $ head x) (fromSql $ x !! 1) (fromSql $ x !! 2)

--  Return the date of the last movie from the Database
extractLastDateFromDataBase :: Connection -> IO (Maybe String)
extractLastDateFromDataBase conn = do
  result <- quickQuery' conn "SELECT MAX(movies.release) FROM movies" []
  if result == [[]] then return Nothing else return $ fromSql $ head $ head result
