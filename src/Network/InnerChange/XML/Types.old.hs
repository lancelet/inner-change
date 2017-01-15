{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DeriveFunctor        #-}

module Network.InnerChange.XML.OldTypes
    {-
    (
      -- * Simple typeclasses
      IsText (toText, fromText)
    , IsElement (toElement{-, fromElement-})
      -- * Types
    , Result (Success, Failure)
    , FailureDetails (FailureOther)
    )-} where

import           Data.Map     (Map)
import qualified Data.Map     as Map (singleton, union, lookup)
import           Data.Maybe   (mapMaybe)
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

-- | Encoding for type 'a' as 'Text'.
--
--   A type which is a member of this typeclass can be used as the content of
--   both:
--
--     - an XML attribute (ie. '<SomeElement attribute="value_goes_here"/>')
--     - text of an XML node (ie. '<SomeElement>value_goes_here</SomeElement>')
class IsText a where
    toText :: a -> Text
    fromText :: Text -> Maybe a

-- | Encoding of a type 'a' as an XML 'Element'.
class IsElement a where
    elementName :: (Proxy a) -> XML.Name
    toElement :: a -> XML.Element
    fromElement :: XML.Element -> Result a

    default elementName :: (Generic a, GHasElementName (Rep a)) => (Proxy a) -> XML.Name
    elementName _ = gelementName $ from (undefined :: a)

    default toElement :: (Generic a, GToElement (Rep a)) => a -> XML.Element
    toElement a = gToElement $ from a

    default fromElement :: (Generic a, GFromElement (Rep a)) => XML.Element -> Result a
    fromElement e = to <$> gFromElement e

-- | Encoding for a type 'a' as an XML 'Node'.
class IsNode a where
    toNode :: a -> XML.Node
    fromNode :: XML.Node -> Result a

-------------------------------------------------------------------------------

showText :: (Show a) => a -> Text
showText = Text.pack . show

readText :: (Read a) => Text -> Maybe a
readText = Read.readMaybe . Text.unpack

instance IsText Text where
    toText = id
    fromText = Just . id

instance IsText Int where
    toText = showText
    fromText = readText

instance {-# OVERLAPS #-} IsNode Text where
    toNode = XML.NodeContent
    fromNode n = case n of
        XML.NodeContent t -> Success t
        _ -> failNodeType NTContent

-------------------------------------------------------------------------------

-- | Result of reading a type from some part of an XML structure.
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

-- | Details of a failure relating to reading a type.
data FailureDetails
    = FailureOther Text
    | FailureNodeType ExpectedNodeType
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

failNonUniqueElement :: XML.Name -> XML.Name -> Result a
failNonUniqueElement parent child = Failure
                                  $ FailureNonUniqueElement
                                  (ElementName parent)
                                  (ElementName child)

failNodeType :: ExpectedNodeType -> Result a
failNodeType t = Failure $ FailureNodeType t

failContent :: XML.Element -> Result a
failContent e = Failure $ FailureContent (elName e)

failElementName :: XML.Name -> XML.Name -> Result a
failElementName actual expected = Failure
                                $ FailureElementName
                                (ElementName actual)
                                (ElementName expected)

failParseContent :: XML.Name -> Text -> Result a
failParseContent e t = Failure
                     $ FailureParseContent
                     (ElementName e)
                     t

failMissingContent :: XML.Name -> Result a
failMissingContent e = Failure $ FailureMissingContent (ElementName e)

failMissingChild :: XML.Name -> XML.Name -> Result a
failMissingChild parent child = Failure
                              $ FailureMissingChild
                              (ElementName parent)
                              (ElementName child)

failMissingAttribute :: XML.Name -> XML.Name -> Result a
failMissingAttribute parent attribute = Failure
                                      $ FailureMissingAttribute
                                      (ElementName parent)
                                      (AttributeName attribute)

failParseAttribute :: XML.Name -> XML.Name -> Text -> Result a
failParseAttribute parent attribute value = Failure
                                          $ FailureParseAttribute
                                          (ElementName parent)
                                          (AttributeName attribute)
                                          value

newtype ElementName = ElementName XML.Name
                    deriving (Eq, Show)

newtype AttributeName = AttributeName XML.Name
                      deriving (Eq, Show)

-- | Returns the 'ElementName' of an XML 'Element'.
elName :: XML.Element -> ElementName
elName = ElementName . XML.elementName

data ExpectedNodeType
    = NTElement
    | NTContent
    deriving (Eq, Show)

-- | Converts a 'Maybe a' to a 'Result a' by adding extra failure details.
maybeResult :: Result a -> Maybe a -> Result a
maybeResult failure m = maybe failure Success m

-------------------------------------------------------------------------------

symbolName :: (KnownSymbol s) => Proxy s -> XML.Name
symbolName p = XML.Name (Text.pack $ symbolVal p) Nothing Nothing
    
-------------------------------------------------------------------------------

newtype Attr a = Attr { unAttr :: a } deriving (Eq, Show, Generic)

instance IsElement a => IsNode a where
    toNode = XML.NodeElement . toElement
    fromNode n = case n of
        XML.NodeElement e -> fromElement e
        _ -> failNodeType NTElement

class GToElement a where
    gToElement :: a x -> XML.Element

instance ( KnownSymbol n
         , GAttrs c
         , GNodes c
         ) => GToElement (D1 ('MetaData n x y z) c) where
    gToElement (M1 c) = XML.Element name attrs nodes
      where
        name  = symbolName (Proxy :: Proxy n)
        attrs = ggetAttrs c
        nodes = ggetNodes c


-------------------------------------------------------------------------------
    
class GAttrs a where
    ggetAttrs :: a x -> Map XML.Name Text

instance GAttrs (C1 ('MetaCons name q w) U1) where
    ggetAttrs = mempty

instance GAttrs s => GAttrs (C1 ('MetaCons name q w) s) where
    ggetAttrs (M1 sel) = ggetAttrs sel

instance GAttrs (S1 ('MetaSel ('Just name) q w e) (Rec0 a)) where
    ggetAttrs = mempty

instance {-# OVERLAPS #-}
         ( KnownSymbol n
         , IsText a
         ) => GAttrs (S1 ('MetaSel ('Just n) q w e) (Rec0 (Attr a))) where
    ggetAttrs (M1 (K1 (Attr x))) = Map.singleton name (toText x)
      where
        name = symbolName (Proxy :: Proxy n)

instance (GAttrs a, GAttrs b) => GAttrs (a :*: b) where
    ggetAttrs (a :*: b) = Map.union (ggetAttrs a) (ggetAttrs b)

-------------------------------------------------------------------------------

class GNodes a where
    ggetNodes :: a x -> [XML.Node]

instance GNodes s => GNodes (C1 ('MetaCons name q w) s) where
    ggetNodes (M1 sel) = ggetNodes sel

instance {-# OVERLAPPING #-}
         IsText a
         => GNodes (S1 ('MetaSel ('Just name) q w e) (Rec0 (Attr a))) where
    ggetNodes (M1 (K1 (Attr x))) = []

instance IsNode a => GNodes (S1 ('MetaSel ('Just name) q w e) (Rec0 a)) where
    ggetNodes (M1 (K1 x)) = [toNode x]

instance (GNodes a, GNodes b) => GNodes (a :*: b) where
    ggetNodes (a :*: b) = ggetNodes a ++ ggetNodes b

-------------------------------------------------------------------------------

namedChildren :: XML.Name -> XML.Element -> [XML.Element]
namedChildren name e = mapMaybe getElem (XML.elementNodes e)
  where
    getElem n = case n of
        XML.NodeElement e@(XML.Element en _ _) | en == name -> Just e
        _ -> Nothing

namedChild :: XML.Name -> XML.Element -> Result XML.Element
namedChild name parent = case namedChildren name parent of
    [x] -> Success x
    []  -> failMissingChild eName name
    _   -> failNonUniqueElement eName name
  where
    eName = XML.elementName parent

namedAttribute :: XML.Name -> XML.Element -> Maybe Text
namedAttribute name e = Map.lookup name (XML.elementAttributes e)

textContent :: XML.Element -> Maybe XML.Node
textContent e = XML.NodeContent
            <$> case mapMaybe getContents (XML.elementNodes e) of
    [] -> Nothing
    ts -> Just $ mconcat ts
  where
    getContents n = case n of
        XML.NodeContent t -> Just t
        _ -> Nothing

class GFromElement a where
    gFromElement :: XML.Element -> Result (a x)

instance ( KnownSymbol name
         , GFromElement c
         ) => GFromElement (D1 ('MetaData name x y z) c) where
    gFromElement e = if eName == sName
                     then M1 <$> gFromElement e
                     else failElementName eName sName
      where
        eName = XML.elementName e
        sName = symbolName (Proxy :: Proxy name)

{-
instance (KnownSymbol name) => GFromElement (C1 ('MetaCons name q w) U1) where
    gFromElement e = if eName == sName
                     then Success $ M1 U1
                     else failElementName eName sName
      where
        eName = XML.elementName e
        sName = symbolName (Proxy :: Proxy name)
-}

instance {-# OVERLAPS #-}
         ( KnownSymbol name
         , GFromElement s
         ) => GFromElement (C1 ('MetaCons name q w) s) where
    gFromElement e = if eName == sName
                     then M1 <$> gFromElement e
                     else failElementName eName sName
      where
        eName = XML.elementName e
        sName = symbolName (Proxy :: Proxy name)

instance {-# OVERLAPS #-}
         ( IsElement a
         , Generic a
         ) => GFromElement (S1 ('MetaSel n q w e) (Rec0 a)) where
    gFromElement e = namedChild cName e
                 >>= fromElement
                 >>= (Success . M1 . K1)
      where
        eName = XML.elementName e
        cName = elementName (Proxy :: Proxy a)
        failure = failMissingChild eName cName


class GHasElementName a where
    gelementName :: a x -> XML.Name

instance (KnownSymbol name) => GHasElementName (D1 ('MetaData name x y z) c) where
    gelementName _ = symbolName (Proxy :: Proxy name)
    

instance {-# OVERLAPS #-}
         ( KnownSymbol name
         , IsText a
         ) => GFromElement (S1 ('MetaSel ('Just name) q w e) (Rec0 (Attr a))) where
    gFromElement e = maybeResult failureMissing (namedAttribute aName e)
                 >>= \text -> maybeResult (failureParse text) (fromText text)
                 >>= (Success . M1 . K1 . Attr)
      where
        failureMissing = failMissingAttribute eName aName
        failureParse = failParseAttribute eName aName
        eName = XML.elementName e
        aName = symbolName (Proxy :: Proxy name)

instance {-# OVERLAPS #-}
         ( IsNode a
         ) => GFromElement (S1 ('MetaSel ('Just name) q w e) (Rec0 a)) where
    gFromElement e = maybeResult failureMissing (textContent e)
                 >>= fromNode 
                 >>= (Success . M1 . K1)
      where
        failureMissing = failMissingContent eName
        eName = XML.elementName e

instance (GFromElement a, GFromElement b) => GFromElement (a :*: b) where
    gFromElement e = do
        l <- gFromElement e
        r <- gFromElement e
        Success $ l :*: r

-------------------------------------------------------------------------------

data X = X
    { test :: Attr Int
    } deriving (Eq, Show, Generic)

instance IsElement X

data Y = Y
    { a :: Attr Text
    , b :: X
    } deriving (Eq, Show, Generic)

instance IsElement Y

data Z = Z
    { q :: Attr Int
    , r :: Attr Text
    , someChildText :: Text
    } deriving (Eq, Show, Generic)

instance IsElement Z

x = X (Attr 42)

y = Y (Attr "a value") x

z = Z (Attr 42) (Attr "r value") "Yellow world"

elemx = toElement x
mbx = fromElement elemx :: Result X

elemy = toElement y
mby = fromElement elemy :: Result Y

elemz = toElement z
mbz = fromElement elemz :: Result Z
