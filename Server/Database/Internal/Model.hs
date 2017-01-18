{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Database.Internal.Model where

import qualified Data.Text              as S
import qualified Data.Text.Lazy         as T
import           Database.Persist.TH

type Mess = (T.Text,  -- message
             T.Text)  -- picture

confAddr :: S.Text
confAddr = "./Database/message.sqlite3"

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
   User
        username T.Text  
        password T.Text
        UniqueNm username
        deriving Show
   Message
        day      T.Text
        mess     [Mess]
        UniqueD  day
        deriving Show
  |]

