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

module Network.InnerChange.XML.Types
    ( -- * Classes
      ToElement (toElement)
    , FromElement (fromElement)
    , ToText (toText)
    , FromText (fromText)
    , ToNode (toNode)
    , FromNode (fromNode)
      -- * Types
    , Result (Success, Failure)
    , FailureDetails
        ( FailureExpectedElement
        , FailureParseAttribute
        , FailureMissingNode
        , FailureParseContent
        , feeExpected
        , feeActual
        , fpaAttribute
        , fpaValue
        , fmnExpected
        , fpcText )
    , ElementName (ElementName)
    , AttributeName (AttributeName)
    , NodeType (NodeTypeElement, NodeTypeContent)
      -- * Generics
    , genericToElement
    , genericFromElement
      -- * Helpers
    , toTextToNode
    , fromTextFromNode
    ) where

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
    toElement = genericToElement

class FromElement a where
    fromElement :: XML.Element -> Result a

    default fromElement :: (Generic a, GFromElement (Rep a))
                        => XML.Element
                        -> Result a
    fromElement = genericFromElement

class ToText a where
    toText :: a -> Text

class FromText a where
    fromText :: Text -> Maybe a
    
class ToNode a where
    toNode :: a -> [XML.Node]

class FromNode a where
    fromNode :: [XML.Node] -> (Result a, [XML.Node])

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
    = FailureExpectedElement
    { feeExpected :: ElementName
    , feeActual   :: ElementName }
    | FailureParseAttribute
    { fpaAttribute :: AttributeName
    , fpaValue     :: Maybe Text }
    | FailureMissingNode
    { fmnExpected :: NodeType }
    | FailureParseContent
    { fpcText :: Text }
    deriving (Eq, Show)

newtype ElementName = ElementName XML.Name
                    deriving (Eq, Show)

newtype AttributeName = AttributeName XML.Name
                      deriving (Eq, Show)

maybeFail :: FailureDetails -> Maybe a -> Result a
maybeFail fd = maybe (Failure fd) Success

data NodeType
    = NodeTypeElement
    | NodeTypeContent
    deriving (Eq, Show)

-------------------------------------------------------------------------------

class ToAttrValue a where
    toAttrValue :: a -> Maybe Text

class FromAttrValue a where
    fromAttrValue :: Maybe Text -> Maybe a

-------------------------------------------------------------------------------

instance ToElement a => ToNode a where
    toNode = (:[]) . XML.NodeElement . toElement

toTextToNode :: (ToText a) => a -> [XML.Node]
toTextToNode = (:[]) . XML.NodeContent . toText

instance {-# OVERLAPS #-} ToNode Text where
    toNode = toTextToNode

instance {-# OVERLAPS #-} ToElement a => ToNode (Maybe a) where
    toNode mba = case mba of
        Just x  -> [(XML.NodeElement . toElement) x]
        Nothing -> []

instance {-# OVERLAPS #-} ToElement a => ToNode [a] where
    toNode = fmap (XML.NodeElement . toElement) . reverse

instance {-# OVERLAPS #-} ToText a => ToAttrValue a where
    toAttrValue = Just . toText

instance {-# OVERLAPS #-} ToAttrValue a => ToAttrValue (Maybe a) where
    toAttrValue = (=<<) toAttrValue

instance {-# OVERLAPS #-} FromText a => FromAttrValue a where
    fromAttrValue = (=<<) fromText

instance FromAttrValue a => FromAttrValue (Maybe a) where
    fromAttrValue mbt = case mbt of
        Nothing -> Just Nothing
        Just t  -> Just <$> fromAttrValue (Just t)

fromTextFromNode :: (FromText a) => [XML.Node] -> (Result a, [XML.Node])
fromTextFromNode ns = case ns of
    (XML.NodeContent t) : ns' -> case fromText t of
        Just x -> (Success x, ns')
        Nothing -> (Failure $ FailureParseContent t, ns)
    _ -> (Failure $ FailureMissingNode NodeTypeContent, ns)

instance FromNode Text where
    fromNode = fromTextFromNode

instance {-# OVERLAPS #-} FromElement a => FromNode a where
    fromNode ns = case ns of
        (XML.NodeElement e) : ns' -> case fromElement e of
            Success v -> (Success v, ns')
            Failure f -> (Failure f, ns)
        _ -> (Failure $ FailureMissingNode NodeTypeElement, ns)

optionalNonFailure :: FailureDetails -> Bool
optionalNonFailure f = case f of
    FailureMissingNode _       -> True
    FailureExpectedElement _ _ -> True
    _                          -> False
    
instance FromNode a => FromNode (Maybe a) where
    fromNode nodes = case nodes of
        [] -> (Success Nothing, [])
        ns -> case fromNode ns of
                  (Success v, ns')
                      -> (Success $ Just v, ns')
                  (Failure f, ns') | optionalNonFailure f
                      -> (Success Nothing, ns')
                  (Failure f, ns')
                      -> (Failure f, ns')

instance FromNode a => FromNode [a] where
    fromNode nodes' = accum [] nodes'
      where
        accum xs nodes = case nodes of
            [] -> (Success (reverse xs), [])
            ns -> case fromNode ns of
                (Success x, ns')
                    -> accum (x:xs) ns'
                (Failure f, ns') | optionalNonFailure f
                    -> (Success xs, ns')
                (Failure f, ns')
                    -> (Failure f, ns')
    
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

freeze :: PartialElement -> XML.Element
freeze pe = XML.Element name (peAttrs pe) ((reverse . peNodes) pe)
  where
    name = fromMaybe (XML.Name "" Nothing Nothing) (peName pe)

toPartial :: XML.Element -> PartialElement
toPartial (XML.Element name as ns) = PartialElement (Just name) as ns

peJustName :: PartialElement -> XML.Name
peJustName pe = fromMaybe emptyName (peName pe)
  where
    emptyName = XML.Name "" Nothing Nothing

whenElement :: XML.Name
            -> (PartialElement -> Result (a x, PartialElement))
            -> PartialElement
            -> Result (a x, PartialElement)
whenElement name f pe = if (eName == name) then f pe else failure
  where
    eName = peJustName pe
    failure = Failure
            $ FailureExpectedElement (ElementName name) (ElementName eName)

takeAttribute :: FromAttrValue a
              => XML.Name
              -> PartialElement
              -> Result (a, PartialElement)
takeAttribute name pe = maybeFail failureParse (fromAttrValue mbvalue)
                    >>= \x -> Success (x, pe)
  where
    mbvalue = Map.lookup name (peAttrs pe)
    failureParse = FailureParseAttribute (AttributeName name) mbvalue

takeNodes :: FromNode a
          => PartialElement
          -> Result (a, PartialElement)
takeNodes pe = rvalue
           >>= \value -> Success (value, pe { peNodes = nodes' })
  where
    (rvalue, nodes') = fromNode (peNodes pe)

-------------------------------------------------------------------------------

-- General functions:
symbolName :: (KnownSymbol s) => Proxy s -> XML.Name
symbolName p = XML.Name (Text.pack $ symbolVal p) Nothing Nothing

-------------------------------------------------------------------------------

genericToElement :: (Generic a, GToElement (Rep a)) => a -> XML.Element
genericToElement x = freeze $ gToElement (from x) emptyPartialElement

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
         ) => GToElement (S1 ('MetaSel ('Just name) q w e)
                             (Rec0 (Attr a))) where
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

genericFromElement :: (Generic a, GFromElement (Rep a))
                   => XML.Element
                   -> Result a
genericFromElement e = (to . fst) <$> gFromElement (toPartial e)

m1fst :: (f p, e) -> (M1 i c f p, e)
m1fst (x, y) = (M1 x, y)

k1fst :: (c, e) -> (K1 i c p, e)
k1fst (x, y) = (K1 x, y)

attrfst :: (a, e) -> (Attr a, e)
attrfst (x, y) = (Attr x, y)
    
class GFromElement a where
    gFromElement :: PartialElement -> Result (a x, PartialElement)

instance ( KnownSymbol name
         , GFromElement c
         ) => GFromElement (D1 ('MetaData name x y z) c) where
    gFromElement = fmap m1fst
                 . whenElement (symbolName (Proxy @name)) gFromElement

instance GFromElement s
         => GFromElement (C1 ('MetaCons name q w) s) where
    gFromElement = fmap m1fst . gFromElement

instance {-# OVERLAPS #-}
         ( KnownSymbol name
         , FromAttrValue a
         ) => GFromElement (S1 ('MetaSel ('Just name) q w e)
                               (Rec0 (Attr a))) where
    gFromElement = fmap (m1fst . k1fst . attrfst)
                 . takeAttribute (symbolName (Proxy @name))

instance FromNode a
         => GFromElement (S1 ('MetaSel ('Just name) q w e) (Rec0 a)) where
    gFromElement = fmap (m1fst . k1fst) . takeNodes

instance ( GFromElement a
         , GFromElement b
         ) => GFromElement (a :*: b) where
    gFromElement e = do
        (l, e')  <- gFromElement e
        (r, e'') <- gFromElement e'
        Success (l :*: r, e'')
    
-------------------------------------------------------------------------------

-- Testing:

data X = X { xa :: Attr Text } deriving (Eq, Show, Generic)
instance ToElement X
instance FromElement X
x = X (Attr "Hello World")
xe = toElement x
x' = (fromElement xe) :: Result X

data Y = Y { ya :: Attr (Maybe Text) } deriving (Eq, Show, Generic)
instance ToElement Y
instance FromElement Y
y1 = Y (Attr (Just "Present"))
y2 = Y (Attr Nothing)
y1e = toElement y1
y2e = toElement y2
y1' = fromElement y1e :: Result Y
y2' = fromElement y2e :: Result Y

data Z = Z
    { za :: Attr Text
    , zy :: Maybe Y
    } deriving (Eq, Show, Generic)
instance ToElement Z
instance FromElement Z
z1 = Z (Attr "z1 attribute") (Just y1)
z2 = Z (Attr "z2 attribute") Nothing
z1e = toElement z1
z2e = toElement z2
z1' = fromElement z1e :: Result Z
z2' = fromElement z2e :: Result Z

data Q1 = Q1
    { q1ys :: [Y]
    } deriving (Eq, Show, Generic)
instance ToElement Q1
instance FromElement Q1
q1a = Q1 [y1, y2]
q1b = Q1 []
q1ae = toElement q1a
q1be = toElement q1b
q1a' = fromElement q1ae :: Result Q1
q1b' = fromElement q1be :: Result Q1

data Q2 = Q2
    { q2txt :: Text
    } deriving (Eq, Show, Generic)
instance ToElement Q2
instance FromElement Q2
q2 = Q2 "Hello"
q2e = toElement q2
q2' = fromElement q2e :: Result Q2

