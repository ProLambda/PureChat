--------------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.Message where

--------------------------------------------------------------------------------
import  qualified Data.Text.Lazy as T
import  Control.Monad.IO.Class
import  Database.Internal.Model
import  Database.Persist
import  Database.Persist.Sqlite
import  Data.Time.Clock
--------------------------------------------------------------------------------

getIndex :: UTCTime
         -> T.Text
getIndex t = T.pack $ take 10 $ show t


updateMes :: [T.Text]
          -> IO ()
updateMes text = runSqlite confAddr $ do
  runMigration migrateAll
  time <- liftIO $ getCurrentTime
  let index = getIndex time
  query <- getBy $ UniqueD index
  case query of
    Nothing -> do
      _ <- insert $ Message index (map trans text)
      return ()
    Just (Entity uid (Message _ mess)) ->
      update uid [MessageMess =. (map trans text) ++ mess]
    where
      trans x = (x, "")


getTextM :: T.Text
         -> IO [T.Text]
getTextM time = runSqlite confAddr $ do
  runMigration migrateAll
  x <- selectFirst [MessageDay ==. time] []
  case x of
    Nothing -> return []
    Just (Entity _ (Message _ xs)) -> return $ map fst xs
  
