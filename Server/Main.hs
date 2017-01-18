-- Following code based on Jasper Van der Jeugt's original work, thank you sir
-- https://github.com/jaspervdj/websockets
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main where

--------------------------------------------------------------------------------
import Control.Monad.IO.Class
import Control.Concurrent           (MVar, forkIO, newMVar, modifyMVar_,
                                     modifyMVar, readMVar, threadDelay)
import Control.Monad                (forM_, forever)
import Control.Exception            (finally)
import Data.Monoid                  (mappend)
import Data.Text                    as T hiding (map, filter, any)
import Data.Time.Clock
import qualified Data.Text.Lazy     as TL
import qualified Network.WebSockets as WS
import Database.Message
--------------------------------------------------------------------------------

type Client      = (Text, WS.Connection)

type ServerState = [Client]

type MsgCache    = [TL.Text]

newServerState :: ServerState
newServerState = []

newMsgCache :: MsgCache
newMsgCache = []

clientExists :: Client
             -> ServerState
             -> Bool
clientExists client = any ((== fst client) .fst)


addClient :: Client
          -> ServerState
          -> ServerState
addClient client clients = client : clients


addMsg :: TL.Text
       -> MsgCache
       -> MsgCache
addMsg msg [] = [msg]
addMsg msg xs = xs ++ [msg]


removeClient :: Client
             -> ServerState
             -> ServerState
removeClient client = filter ((/= fst client) . fst)


broadcast :: Text
          -> ServerState
          -> IO ()
broadcast message clients = 
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message


loop :: MVar MsgCache
     -> IO ()
loop msgc = do
  msgs <- readMVar msgc
  updateMes msgs
  modifyMVar_ msgc $ \_ -> return newMsgCache -- clean cache
  threadDelay $ 3600 * 1000000                -- per hour
  

main :: IO ()
main = do
  state <- newMVar newServerState
  msgs  <- newMVar newMsgCache
  _     <- forkIO . forever $ loop msgs
  WS.runServer "0.0.0.0" 8888 $ app state msgs


app :: MVar ServerState
    -> MVar MsgCache
    -> WS.ServerApp
app state msgs pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30             -- ensure the connection stays alive
  (msg :: Text) <- WS.receiveData conn
  clients <- readMVar state
  case msg of
    _ | clientExists client clients ->
          WS.sendTextData conn ("User already exists" :: Text)
      | otherwise -> flip finally disconnect $ do
          modifyMVar_ state $ \s -> do
            let s' = addClient client s
            WS.sendTextData conn $
              "Hello! Users: " `mappend` T.intercalate ", " (map fst s)
            broadcast (fst client `mappend` " joined") s'
            return s'
          talk conn state msgs client
        where
          client     = (msg, conn)
          disconnect = do
            s <- modifyMVar state $ \s ->
              let s' = removeClient client s in return (s', s')
            broadcast (fst client `mappend` " disconnected") s


talk :: WS.Connection
     -> MVar ServerState
     -> MVar MsgCache
     -> Client
     -> IO ()
talk conn state msgs (user, _) = forever $ do
  msg <- WS.receiveData conn
  ct  <- liftIO $ getCurrentTime
  let time = T.drop 11 $ T.take 16 $ T.pack $ show ct
      msg' = user `mappend` "@" `mappend` time `mappend` ": " `mappend` msg
  modifyMVar_ msgs $ \x -> return $ addMsg (TL.fromStrict msg') x
  readMVar state >>= broadcast msg'
