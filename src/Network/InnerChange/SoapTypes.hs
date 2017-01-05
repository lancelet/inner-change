{-# LANGUAGE OverloadedStrings #-}

module Network.InnerChange.SoapTypes where

import qualified Data.Map.Strict as Map (empty)
import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.Lazy  as LT (toStrict)
import qualified Text.XML        as XML (Document (Document), Element (Element),
                                         Name, Node (NodeContent),
                                         Prologue (Prologue), def, renderText)


-- $setup
-- DocTest setup:
-- >>> :set -XOverloadedStrings
    

-- | Renders an XML Element as a Text string.
--
-- >>> elemToText $ XML.Element "Element" Map.empty []
-- "<Element/>"
elemToText :: XML.Element -> Text
elemToText e = snd
             $ T.breakOnEnd "?>"  -- drop the XML prolog
             $ LT.toStrict
             $ XML.renderText XML.def doc
  where
    doc = XML.Document prologue e []
    prologue = XML.Prologue [] Nothing []


-- | Creates an XML Element with a given name and content.
--
-- The XML element has no attributes.
--
-- >>> elemToText $ txtElement "Foo" "Bar"
-- "<Foo>Bar</Foo>"
txtElement :: XML.Name -> Text -> XML.Element
txtElement name content = XML.Element name Map.empty [ XML.NodeContent content ]


-- | Serialization of some type 'a' to an 'XML.Element'.
class ToElement a where
    toElement :: a -> XML.Element


-- | Set of properties to return in an item or folder response.
--
-- https://msdn.microsoft.com/en-us/library/office/aa580545(v=exchg.150).aspx
--
-- >>> elemToText . toElement $ BaseShape IdOnly
-- "<BaseShape>IdOnly</BaseShape>"
data BaseShape = IdOnly
               | Default
               | AllProperties
               deriving (Eq, Show)

instance ToElement BaseShape where
    toElement b = txtElement "BaseShape" $ case b of
                                               IdOnly        -> "IdOnly"
                                               Default       -> "Default"
                                               AllProperties -> "AllProperties"
