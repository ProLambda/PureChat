-- Following code based on Jasper Van der Jeugt's original work, thank you sir
-- https://github.com/jaspervdj/websockets
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main where

--------------------------------------------------------------------------------
import  Control.Monad.IO.Class
import  Control.Concurrent           (MVar, forkIO, newMVar, modifyMVar_,
                                      modifyMVar, readMVar, threadDelay)
import  Control.Monad                (forM_, forever)
import  Control.Exception            (finally, try)
import  Data.Monoid                  (mappend)
import  Data.Text                    as T hiding (map, filter, any)
import  Data.Time.Clock
import  qualified Data.Text.Lazy     as TL
import  qualified Network.WebSockets as WS
import  Data.ByteString.Lazy         as L hiding (map, filter, any)
import  Database.Message
import  GHC.Generics
import  Data.Aeson
import  Network.HTTP.Client          (parseRequest, HttpException)
--------------------------------------------------------------------------------

data ServerMsg = TEXT Text | PIC Text | JOIN Text | LEAVE Text |
                 ERROR     | LIST [ServerMsg]
  deriving (Generic, Show, Eq)

instance ToJSON ServerMsg where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON ServerMsg

packt :: Text -> ServerMsg
packt = TEXT

packp :: Text -> ServerMsg
packp = PIC

packj :: Text -> ServerMsg
packj = JOIN

packl :: Text -> ServerMsg
packl = LEAVE 

packs :: [ServerMsg] -> ServerMsg
packs = LIST

err :: ServerMsg
err = ERROR

userList :: [Client] -> L.ByteString
userList [] = ""
userList xs = encode . packs $ map (packj . fst) xs

cacheMsg :: [ServerMsg] -> L.ByteString
cacheMsg [] = ""
cacheMsg xs = encode $ packs xs

suffix :: [Text]
suffix = [".jpg", ".png", ".gif"]

checkImg :: Text -> ServerMsg
checkImg img = if T.drop (T.length img - 4) img `Prelude.elem` suffix
                  then packp img
                  else err

parsePic :: Text -> IO ServerMsg
parsePic url = 
  if T.take 4 url == "http"
     then do par <- try . parseRequest $ T.unpack url
             case par of
               Left  e -> do
                 print (e :: HttpException)
                 return err
               Right _ -> return $ checkImg url
     else return err

transM :: ServerMsg -> TL.Text
transM s = case s of
             TEXT t -> TL.fromStrict t
             PIC  p -> TL.fromStrict p
             _      -> ""
--------------------------------------------------------------------------------

type Client      = (Text, WS.Connection)

type ServerState = [Client]

type MsgCache    = [ServerMsg]

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


addMsg :: ServerMsg
       -> MsgCache
       -> MsgCache
addMsg msg [] = [msg]
addMsg msg xs = xs ++ [msg]


removeClient :: Client
             -> ServerState
             -> ServerState
removeClient client = filter ((/= fst client) . fst)


broadcast :: ServerMsg
          -> ServerState
          -> IO ()
broadcast message clients =
  forM_ clients $ \(_, conn) -> WS.sendTextData conn (encode message)


loop :: MVar MsgCache
     -> IO ()
loop msgc = do
  msgs <- readMVar msgc
  updateMes (filter (/= "") $ map transM msgs)
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
  conn    <- WS.acceptRequest pending
  WS.forkPingThread conn 30             -- ensure the connection stays alive
  msg     <- WS.receiveData conn
  clients <- readMVar state
  m'      <- readMVar msgs
  case msg of
    _ | clientExists client clients ->
          WS.sendTextData conn (encode $ packt "User already exists")
      | otherwise -> flip finally disconnect $ do    
          modifyMVar_ state $ \s -> do
            let s' = addClient client s
            WS.sendTextData conn (userList s')
            WS.sendTextData conn (cacheMsg m')
            broadcast (packt (msg `mappend` " joined")) s'
            broadcast (packj msg) s   -- user list request for joining
            return s'
          talk conn state msgs client
        where
          client     = (msg, conn)
          disconnect = do
            s <- modifyMVar state $ \s ->
              let s' = removeClient client s in return (s', s')
            broadcast (packt $ fst client `mappend` " disconnected") s
            broadcast (packl msg) s   -- user list request for leaving


talk :: WS.Connection
     -> MVar ServerState
     -> MVar MsgCache
     -> Client
     -> IO ()
talk conn state msgs (user, _) = forever $ do
  msg <- WS.receiveData conn
  par <- parsePic msg
  ct  <- liftIO getCurrentTime
  let time = T.drop 11 $ T.take 16 $ T.pack $ show ct
      temp = user `mappend` "@" `mappend` time `mappend` ": "
      msg' = if par == err
                then packt (temp `mappend` msg)
                else packt temp
  modifyMVar_ msgs $ \x -> return $ addMsg msg' x
  if par == err
     then    readMVar state >>= broadcast msg'
     else do readMVar state >>= broadcast (packs [msg', packp msg])
             print (packp msg)
             modifyMVar_ msgs $ \x -> return $ addMsg (packp msg) x
