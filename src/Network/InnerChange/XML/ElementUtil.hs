{-# OPTIONS_GHC -Wwarn #-}

module Network.InnerChange.XML.ElementUtil

where

import           Network.InnerChange.XML.Types (AttributeName (AttributeName),
                                                FromAttrValue', FromNodes',
                                                Result', ToAttrValue, ToNode,
                                                fromAttrValue',
                                                fromNodesKnownOrder,
                                                fromNodesUnknownOrder,
                                                toAttrValue, toNode,
                                                rootFail,
                                                RootFailure (FailExpectedElement),
                                                ElementName (ElementName))

import           Data.Map                      (Map)
import qualified Data.Map                      as Map (delete, insert, lookup)
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Text.XML                      as XML (Element (Element),
                                                       Name (Name), Node)

-------------------------------------------------------------------------------

data BuildElement = BuildElement
    { beName  :: XML.Name
    , beAttrs :: Map XML.Name Text
    , beNodes :: [XML.Node]         -- nodes stored in reverse order
    } deriving (Show)

data UnbuildElement = UnbuildElement
    { ueName  :: XML.Name
    , ueAttrs :: Map XML.Name Text
    , ueNodes :: [XML.Node]         -- nodes stored in forward order
    } deriving (Show)

-------------------------------------------------------------------------------

freeze :: BuildElement -> XML.Element
freeze (BuildElement n as ns) = XML.Element n as (reverse ns)

thaw :: XML.Element -> UnbuildElement
thaw (XML.Element n as ns) = UnbuildElement n as ns

-------------------------------------------------------------------------------

addTextAttr :: XML.Name -> Text -> BuildElement -> BuildElement
addTextAttr name value (BuildElement n as ns) = BuildElement n as' ns
  where
    as' = Map.insert name value as

addAttr :: ToAttrValue a => XML.Name -> a -> BuildElement -> BuildElement
addAttr name value = maybe id (addTextAttr name) (toAttrValue value)

prependRawNodes :: [XML.Node] -> BuildElement -> BuildElement
prependRawNodes nodes (BuildElement n as ns) = BuildElement n as (nodes' <> ns)
  where
    nodes' = reverse nodes

prependNodes :: ToNode a => a -> BuildElement -> BuildElement
prependNodes = prependRawNodes . toNode

setName :: XML.Name -> BuildElement -> BuildElement
setName name (BuildElement _ as ns) = BuildElement name as ns

-------------------------------------------------------------------------------

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

removeTextAttr :: XML.Name -> UnbuildElement -> (Maybe Text, UnbuildElement)
removeTextAttr name (UnbuildElement n as ns) = (attr, UnbuildElement n as' ns)
  where
    attr = Map.lookup name as
    as'  = Map.delete name as

removeAttr :: FromAttrValue' a
           => XML.Name
           -> UnbuildElement
           -> (Result' a, UnbuildElement)
removeAttr name be = mapFst (fromAttrValue' (AttributeName name))
                   $ removeTextAttr name be

removeNodesKnownOrder :: FromNodes' a
                      => UnbuildElement
                      -> (Result' a, UnbuildElement)
removeNodesKnownOrder (UnbuildElement n as ns) = (r, UnbuildElement n as ns')
  where
    (r, ns') = fromNodesKnownOrder ns

removeNodesUnknownOrder :: FromNodes' a
                        => UnbuildElement
                        -> (Result' a, UnbuildElement)
removeNodesUnknownOrder (UnbuildElement n as ns) = (r, UnbuildElement n as ns')
  where
    (r, ns') = fromNodesUnknownOrder ns

whenElementName :: XML.Name
                -> (UnbuildElement -> Result' (a, UnbuildElement))
                -> UnbuildElement
                -> Result' (a, UnbuildElement)
whenElementName name f e =
    if ueName e == name
    then f e
    else rootFail (FailExpectedElement (ElementName name))
