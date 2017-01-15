-- {-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.InnerChange.EWS.Types where

import           Network.InnerChange.XML.Types

import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)
import qualified Text.XML                      as XML (Element)

newtype Id = Id { unId :: Text }
             deriving (Eq, Show, Generic, ToText, FromText)

newtype ChangeKey = ChangeKey { unChangeKey :: Text }
                    deriving (Eq, Show, Generic, ToText, FromText)

-------------------------------------------------------------------------------

data ItemId = ItemId Id (Maybe ChangeKey)
            deriving (Eq, Show, Generic)

instance ToElement ItemId where
    toElement (ItemId i c) = toElement $ SoapItemId (Attr i) (Attr c)

instance FromElement ItemId where
    fromElement e = case fromElement e of
        Success (SoapItemId i c) -> Success $ ItemId (unAttr i) (unAttr c)
        Failure f                -> Failure f

{-
-- instead, would be nice to write:
instance Iso ItemId SoapItemId
instance ToElement ItemId where toElement = toElement . isoTo
instance FromElement ItemId where fromElement = fmap isoFrom . fromElement

-- or magick it up automatically using TH
deriveElement ''ItemId SoapItemId
-}

-------------------------------------------------------------------------------
    
data SoapItemId = SoapItemId
    { id        :: Attr Id
    , changeKey :: Attr (Maybe ChangeKey)
    } deriving (Eq, Show, Generic)
instance ToElement SoapItemId
instance FromElement SoapItemId

-------------------------------------------------------------------------------

-- temporary test stuff
    
testItemId1, testItemId2 :: ItemId
testItemId1 = ItemId (Id "ABBA") Nothing
testItemId2 = ItemId (Id "BABBA") (Just (ChangeKey "CAFEBABE"))

e1, e2 :: XML.Element
e1 = toElement testItemId1
e2 = toElement testItemId2

read1, read2 :: Result ItemId
read1 = fromElement e1
read2 = fromElement e2
