{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications     #-}

-- TODO Proper export
module Network.InnerChange.XML.Types where

import           Data.Map     (Map)
import qualified Data.Map     as Map (singleton, union, lookup)
import           Data.Maybe   (mapMaybe, fromMaybe, catMaybes)
import           Data.Monoid  ((<>))
import           Data.Proxy   (Proxy (Proxy))
import           Data.Text    (Text)
import qualified Data.Text    as Text (pack, unpack)
import           GHC.Generics ((:*:) ((:*:)), C1, D1, Generic, K1 (K1), M1 (M1),
                               Meta (MetaCons, MetaSel, MetaData), Rec0, Rep, S1, U1 (U1),
                               from, to, Par1(Par1))
import           GHC.TypeLits (KnownSymbol, symbolVal)
import           Text.Read    (Read)
import qualified Text.Read    as Read (readMaybe)
import qualified Text.XML     as XML (Element (Element), Name (Name),
                                      Node (NodeContent, NodeElement),
                                      elementName, elementNodes,
                                      elementAttributes)

-------------------------------------------------------------------------------

class ToElement a where
    toElement :: a -> XML.Element
    default toElement :: (Generic a, GToElement (Rep a)) => a -> XML.Element
    toElement a = freeze $ gToElement (from a) emptyPartialElement

class FromElement a where
    fromElement :: XML.Element -> Result a

class ToText a where
    toText :: a -> Text

class FromText a where
    fromText :: a -> Maybe Text
    
-------------------------------------------------------------------------------

instance ToText Text where toText = id
instance FromText Text where fromText = Just . id

-------------------------------------------------------------------------------

newtype Attr a = Attr { unAttr :: a } deriving (Eq, Show, Generic)

-------------------------------------------------------------------------------

data Result a
    = Success a
    | Failure FailureDetails
    deriving (Eq, Show, Functor)

instance Applicative Result where
    pure = Success
    Failure f <*> _         = Failure f
    _         <*> Failure f = Failure f
    Success f <*> Success x = Success (f x)

instance Monad Result where
    Failure f >>= _ = Failure f
    Success x >>= f = f x

data FailureDetails
    = FailureOther Text
    | FailureNodeType NodeType
    | FailureContent ElementName
    | FailureElementName
    { fenActual   :: ElementName
    , fenExpected :: ElementName }
    | FailureMissingChild
    { fmcParent :: ElementName
    , fmcChild  :: ElementName }
    | FailureMissingAttribute ElementName AttributeName
    | FailureParseAttribute ElementName AttributeName Text
    | FailureNonUniqueElement
    { fnuParent :: ElementName
    , fnuChild  :: ElementName }
    | FailureMissingContent ElementName
    | FailureParseContent ElementName Text
    deriving (Eq, Show)

newtype ElementName = ElementName XML.Name
                    deriving (Eq, Show)

newtype AttributeName = AttributeName XML.Name
                      deriving (Eq, Show)

data NodeType
    = NodeTypeElement
    | NodeTypeContent
    deriving (Eq, Show)

-------------------------------------------------------------------------------

class ToNode a where
    toNode :: a -> [XML.Node]

class FromNode a where
    fromNode :: [XML.Node] -> (Result a, [XML.Node])

class ToAttrValue a where
    toAttrValue :: a -> Maybe Text

class FromAttrValue a where
    fromAttrValue :: Text -> Maybe a

-------------------------------------------------------------------------------

instance ToElement a => ToNode a where
    toNode = (:[]) . XML.NodeElement . toElement

instance ToNode Text where
    toNode = (:[]) . XML.NodeContent

instance {-# OVERLAPS #-} ToElement a => ToNode (Maybe a) where
    toNode mba = case mba of
        Just x  -> [(XML.NodeElement . toElement) x]
        Nothing -> []

instance {-# OVERLAPS #-} ToElement a => ToNode [a] where
    toNode = fmap (XML.NodeElement . toElement) . reverse

-------------------------------------------------------------------------------

instance {-# OVERLAPS #-} ToText a => ToAttrValue a where
    toAttrValue = Just . toText

instance {-# OVERLAPS #-} ToAttrValue a => ToAttrValue (Maybe a) where
    toAttrValue = (=<<) toAttrValue

instance ToAttrValue a => ToAttrValue [a] where
    toAttrValue = error "ToAttrValue[a] not yet implemented" -- TODO

-------------------------------------------------------------------------------

data PartialElement
    = PartialElement
    { peName  :: Maybe XML.Name
    , peAttrs :: Map XML.Name Text
    , peNodes :: [XML.Node]
    } deriving (Show)

emptyPartialElement :: PartialElement
emptyPartialElement = PartialElement Nothing mempty []

partialElementName :: (KnownSymbol name)
                   => Proxy name
                   -> (PartialElement -> PartialElement)
partialElementName p initPe = initPe { peName = (Just . symbolName) p }

addAttribute :: (KnownSymbol name, ToAttrValue a)
             => Proxy name
             -> a
             -> (PartialElement -> PartialElement)
addAttribute p x initPe = case toAttrValue x of
    Just v -> initPe { peAttrs = Map.union (peAttrs initPe) attr }
      where
        attr = Map.singleton (symbolName p) v
    Nothing -> initPe

addNodes :: ToNode a => a -> (PartialElement -> PartialElement)
addNodes x initPe = case toNode x of
    [n] -> initPe { peNodes = n : (peNodes initPe) }
    ns  -> initPe { peNodes = ns <> (peNodes initPe) }

-- TODO: may need to reverse nodes?
freeze :: PartialElement -> XML.Element
freeze pe = XML.Element name (peAttrs pe) (peNodes pe)
  where
    name = fromMaybe (XML.Name "" Nothing Nothing) (peName pe)

-------------------------------------------------------------------------------

-- General functions:

symbolName :: (KnownSymbol s) => Proxy s -> XML.Name
symbolName p = XML.Name (Text.pack $ symbolVal p) Nothing Nothing
    
-------------------------------------------------------------------------------

class GToElement a where
    gToElement :: a x -> (PartialElement -> PartialElement)

instance ( KnownSymbol name
         , GToElement c
         ) => GToElement (D1 ('MetaData name x y z) c) where
    gToElement (M1 c)
        = (gToElement c) . (partialElementName (Proxy @name))

instance GToElement s => GToElement (C1 ('MetaCons name q w) s) where
    gToElement (M1 sel)
        = gToElement sel

instance {-# OVERLAPS #-}
         ( KnownSymbol name
         , ToAttrValue a
         ) => GToElement (S1 ('MetaSel ('Just name) q w e) (Rec0 (Attr a))) where
    gToElement (M1 (K1 (Attr x)))
        = addAttribute (Proxy @name) x

instance ToNode a
         => GToElement (S1 ('MetaSel ('Just name) q w e) (Rec0 a)) where
    gToElement (M1 (K1 x)) = addNodes x

instance ( GToElement a
         , GToElement b
         ) => GToElement (a :*: b) where
    gToElement (a :*: b) = (gToElement b) . (gToElement a)

-------------------------------------------------------------------------------

-- Testing:

data X = X { xa :: Attr Text } deriving (Eq, Show, Generic)
instance ToElement X
x = X (Attr "Hello World")
xe = toElement x

data Y = Y { ya :: Attr (Maybe Text) } deriving (Eq, Show, Generic)
instance ToElement Y
y1 = Y (Attr (Just "Present"))
y2 = Y (Attr Nothing)
y1e = toElement y1
y2e = toElement y2

data Z = Z
    { za :: Attr Text
    , zy :: Maybe Y
    } deriving (Eq, Show, Generic)
instance ToElement Z
z1 = Z (Attr "z1 attribute") (Just y1)
z2 = Z (Attr "z2 attribute") Nothing
z1e = toElement z1
z2e = toElement z2

data Q1 = Q1
    { q1ys :: [Y]
    } deriving (Eq, Show, Generic)
instance ToElement Q1
q1a = Q1 [y1, y2]
q1b = Q1 []
q1ae = toElement q1a
q1be = toElement q1b
