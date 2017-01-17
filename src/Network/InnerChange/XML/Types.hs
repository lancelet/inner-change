{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Network.InnerChange.XML.Types where

import           Control.Applicative (Alternative, empty, (<|>))
import           Data.Map            (Map)
import qualified Data.Map            as Map (lookup, singleton, union, insert, delete, empty)
import           Data.Maybe          (fromMaybe)
import           Data.Monoid         ((<>))
import           Data.Proxy          (Proxy (Proxy))
import           Data.Text           (Text)
import qualified Data.Text           as Text (pack, unpack)
import           GHC.Generics        ((:*:) ((:*:)), (:+:) (L1, R1), C1, D1,
                                      Generic, K1 (K1), M1 (M1), V1,
                                      Meta (MetaCons, MetaData, MetaSel), Rec0,
                                      Rep, S1, U1 (U1), from, to, unK1, unM1)
import           GHC.TypeLits        (KnownSymbol, symbolVal)
import           Text.Read           (Read)
import qualified Text.Read           as Read (readMaybe)
import qualified Text.XML            as XML (Element (Element), Name (Name),
                                             Node (NodeContent, NodeElement))

-------------------------------------------------------------------------------

class ToElement a where
    toElement :: a -> XML.Element

    default toElement :: (Generic a, GToElement (Rep a)) => a -> XML.Element
    toElement = genericToElement defaultElementOptions

class FromElement a where
    fromElement :: XML.Element -> Result a

    default fromElement :: (Generic a, GFromElement (Rep a))
                        => XML.Element
                        -> Result a
    fromElement = genericFromElement defaultElementOptions

class ToNodes a where
    toNodes :: a -> [XML.Node]

class FromNodes a where
    fromNodesKnownOrder :: SelectorName
                        -> (SelectorName -> ElementName)
                        -> [XML.Node]
                        -> Result (a, [XML.Node])
    fromNodesUnknownOrder :: SelectorName
                          -> (SelectorName -> ElementName)
                          -> [XML.Node]
                          -> Result (a, [XML.Node])

class ToAttrValue a where
    toAttrValue :: a -> Maybe Text

class FromAttrValue a where
    fromAttrValue :: PossibleAttribute -> Result a

-------------------------------------------------------------------------------

newtype Attr a = Attr { unAttr :: a } deriving (Eq, Show, Generic)

data AttributeState = AttributePresent Text
                    | AttributeMissing
                    deriving (Eq, Show)

data PossibleAttribute = PossibleAttribute AttributeName AttributeState 

-------------------------------------------------------------------------------

-- ToNodes instances

instance ToElement a => ToNodes a where
    toNodes x = [XML.NodeElement $ toElement x]

instance {-# OVERLAPS #-} ToNodes Text where
    toNodes text = [XML.NodeContent text]

instance {-# OVERLAPS #-} ToElement a => ToNodes (Maybe a) where
    toNodes = maybe [] (\x -> [XML.NodeElement $ toElement x])

instance {-# OVERLAPS #-} ToElement a => ToNodes [a] where
    toNodes = fmap (XML.NodeElement . toElement)
    
-------------------------------------------------------------------------------

-- ToAttrValue instances

instance ToAttrValue Text where
    toAttrValue = Just

instance {-# OVERLAPS #-} ToAttrValue a => ToAttrValue (Maybe a) where
    toAttrValue = (=<<) toAttrValue

-------------------------------------------------------------------------------

-- FromNodes instances

instance FromNodes Text where
    fromNodesKnownOrder _ _ ((XML.NodeContent text) : ns)
        = Success (text, ns)
    fromNodesKnownOrder selName _ _
        = rootFail (FailExpectedContent selName)

    fromNodesUnknownOrder selName _ nodes
        = maybeFail (FailExpectedContent selName) (concatAllContent nodes)

instance FromElement a => FromNodes a where
    fromNodesKnownOrder _ _ ((XML.NodeElement e) : ns) = undefined  -- TODO
    fromNodesKnownOrder selName mkElementName _
        = rootFail (FailExpectedElement (mkElementName selName))

    fromNodesUnknownOrder selName mkElementName nodes
        = (mapFst readElement) <$>
          failMaybe failMissingChild (uniqueChildElementWithName elementName nodes)
      where
        elementName = mkElementName selName
        readElement = trackFailure fromElement
        failMissingChild = rootFail (FailExpectedElement elementName)
    
-------------------------------------------------------------------------------

-- FromAttrValue instances

instance FromAttrValue Text where
    fromAttrValue = handleRequiredAttribute id

instance FromAttrValue a => FromAttrValue (Maybe a) where
    fromAttrValue pa = case pa of
        AttributeMissing      -> Success Nothing
        AttributePresent text -> fmap Just (fromAttrValue text)

-------------------------------------------------------------------------------

data Result a = Success a
              | Failure [XML.Element] RootFailure
              deriving (Eq, Show, Functor)

instance Applicative Result where
    pure = Success
    Success f    <*> Success x    = Success (f x)
    _            <*> Failure es r = Failure es r
    Failure es r <*> _            = Failure es r

instance Monad Result where
    Failure ea r >>= _ = Failure ea r
    Success x    >>= f = f x

trackFailure :: (XML.Element -> Result a) -> XML.Element -> Result a
trackFailure f e = case f e of
    s@(Success _) -> s
    Failure es r  -> Failure (e : es) r

trackNestedFailure :: (a -> XML.Element) -> (a -> Result b)
                   -> a
                   -> Result b

data RootFailure = FailExpectedElement ElementName
                 | FailExpectedContent SelectorName
                 | FailCouldNotParseAttribute AttributeName Text
                 | FailMissingAttribute AttributeName
                 | FailMissingContent
                 deriving (Eq, Show)

newtype ElementName = ElementName XML.Name
                    deriving (Eq, Show)

newtype AttributeName = AttributeName XML.Name
                      deriving (Eq, Show)

newtype SelectorName = SelectorName Text
                     deriving (Eq, Show)

rootFail :: RootFailure -> Result a
rootFail = Failure []

maybeFail :: RootFailure -> Maybe a -> Result a
maybeFail r = maybe (rootFail r) Success

handleRequiredAttribute :: (Text -> Maybe a) -> PossibleAttribute -> Result a
handleRequiredAttribute f a = case a of
    PossibleAttribute name (AttributePresent text)
        -> maybeFail (FailCouldNotParseAttribute name text) (f text)
    PossibleAttribute name AttributeMissing
        -> rootFail (FailMissingAttribute name)

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

emptyXMLName :: XML.Name
emptyXMLName = XML.Name "" Nothing Nothing

emptyBuildElement :: BuildElement
emptyBuildElement = BuildElement emptyXMLName Map.empty []

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

prependNodes :: ToNodes a => a -> BuildElement -> BuildElement
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

removeAttr :: FromAttrValue a
           => XML.Name
           -> UnbuildElement
           -> (Result a, UnbuildElement)
removeAttr name be = mapFst (fromAttrValue (AttributeName name))
                   $ removeTextAttr name be

removeNodesKnownOrder :: FromNodes a
                      => UnbuildElement
                      -> (Result a, UnbuildElement)
removeNodesKnownOrder (UnbuildElement n as ns) = (r, UnbuildElement n as ns')
  where
    (r, ns') = fromNodesKnownOrder ns

removeNodesUnknownOrder :: FromNodes a
                        => UnbuildElement
                        -> (Result a, UnbuildElement)
removeNodesUnknownOrder (UnbuildElement n as ns) = (r, UnbuildElement n as ns')
  where
    (r, ns') = fromNodesUnknownOrder ns

whenElementName :: XML.Name
                -> (UnbuildElement -> Result (a, UnbuildElement))
                -> UnbuildElement
                -> Result (a, UnbuildElement)
whenElementName name f e =
    if ueName e == name
    then f e
    else rootFail (FailExpectedElement (ElementName name))

-------------------------------------------------------------------------------

concatAllContent :: [XML.Node] -> Maybe (Text, [XML.Node])
concatAllContent = error "concatAllContent not implemented"

allChildElementsWithName :: ElementName -> [XML.Node] -> ([XML.Element], [XML.Node])
allChildElementsWithName = error "allChildElementsWithName"

uniqueChildElementWithName :: ElementName -> [XML.Node] -> Maybe (XML.Element, [XML.Node])
uniqueChildElementWithName = error "uniqueChildElementWithName"

-------------------------------------------------------------------------------

data NodeLayout = NodeLayoutOrdered | NodeLayoutUnOrdered
                deriving (Eq, Show)

data ElementOptions = ElementOptions
    { toElementName    :: Text -> XML.Name
    , toAttributeName  :: Text -> XML.Name
    , toContent        :: Text -> Text
    , nodeLayout       :: NodeLayout }

data AttributeOptions = AttributeOptions
    { constructorToAttribute :: Text -> Text }

defaultElementOptions :: ElementOptions
defaultElementOptions = error "defaultElementOptions"

-------------------------------------------------------------------------------

genericToElement :: (Generic a, GToElement (Rep a))
                 => ElementOptions
                 -> a
                 -> XML.Element
genericToElement opts x = freeze $ gToElement opts (from x) emptyBuildElement
    
genericFromElement :: (Generic a, GFromElement (Rep a))
                   => ElementOptions
                   -> XML.Element
                   -> Result a
genericFromElement opts e = (to . fst) <$> gFromElement opts (thaw e)
    
-------------------------------------------------------------------------------

class GToElement a where
    gToElement :: ElementOptions -> a x -> BuildElement -> BuildElement

-------------------------------------------------------------------------------

class GFromElement a where
    gFromElement :: ElementOptions
                 -> UnbuildElement
                 -> Result (a x, UnbuildElement)
