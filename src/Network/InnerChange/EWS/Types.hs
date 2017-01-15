-- {-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.InnerChange.EWS.Types where

import           Network.InnerChange.XML.Types

import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)

newtype Id = Id { unId :: Text }
             deriving (Eq, Show, Generic, ToText, FromText)

newtype ChangeKey = ChangeKey { unChangeKey :: Text }
                    deriving (Eq, Show, Generic, ToText, FromText)

-------------------------------------------------------------------------------

data ItemId = ItemId Id ChangeKey

instance ToElement ItemId where
    toElement (ItemId i c) = toElement $ SoapItemId (Attr i) (Attr c)

instance FromElement ItemId where
    fromElement e = case fromElement e of
        Success (SoapItemId i c) -> Success $ ItemId (unAttr i) (unAttr c)
        Failure f                -> Failure f

-------------------------------------------------------------------------------
    
data SoapItemId = SoapItemId
    { id        :: Attr Id
    , changeKey :: Attr ChangeKey
    } deriving (Eq, Show, Generic)
instance ToElement SoapItemId
instance FromElement SoapItemId
