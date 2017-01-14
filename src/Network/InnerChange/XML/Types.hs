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

{-
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
-}

module Network.InnerChange.XML.Types
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
import qualified Data.Map     as Map (singleton, union)
import           Data.Proxy   (Proxy (Proxy))
import           Data.Text    (Text)
import qualified Data.Text    as Text (pack)
import           GHC.Generics ((:*:) ((:*:)), C1, D1, Generic, K1 (K1), M1 (M1),
                               Meta (MetaCons, MetaSel, MetaData), Rec0, Rep, S1, U1,
                               from)
import           GHC.TypeLits (KnownSymbol, symbolVal)
import qualified Text.XML     as XML (Element (Element), Name (Name),
                                      Node (NodeContent, NodeElement),
                                      elementName)

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
    toElement :: a -> XML.Element
    -- fromElement :: XML.Element -> Result a

    default toElement :: (Generic a, GIsElement (Rep a)) => a -> XML.Element
    toElement a = gToElement $ from a

-- | Encoding for a type 'a' as an XML 'Node'.
class IsNode a where
    toNode :: a -> XML.Node
    -- fromNode :: XML.Element -> XML.Node -> Result a

-------------------------------------------------------------------------------

instance IsText Text where
    toText = id
    fromText = Just . id

instance IsText Int where
    toText = Text.pack . show
    fromText = error "Not implemented: IsText Int fromText"

instance {-# OVERLAPS #-} IsNode Text where
    toNode = XML.NodeContent

-------------------------------------------------------------------------------

-- | Result of reading a type from some part of an XML structure.
data Result a
    = Success a
    | Failure FailureDetails
    deriving (Eq, Show)

-- | Details of a failure relating to reading a type.
data FailureDetails
    = FailureOther Text
    | FailureNodeType ElementName ExpectedNodeType
    | FailureContent ElementName
    deriving (Eq, Show)

failNodeType :: XML.Element -> ExpectedNodeType -> Result a
failNodeType e t = Failure $ FailureNodeType (elName e) t

failContent :: XML.Element -> Result a
failContent e = Failure $ FailureContent (elName e)

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
maybeResult :: FailureDetails -> Maybe a -> Result a
maybeResult f m = maybe (Failure f) Success m

-------------------------------------------------------------------------------

symbolName :: (KnownSymbol s) => Proxy s -> XML.Name
symbolName p = XML.Name (Text.pack $ symbolVal p) Nothing Nothing
    
-------------------------------------------------------------------------------

newtype Attr a = Attr { unAttr :: a } deriving (Eq, Show, Generic)


{-
newtype Attr a = Attr { unAttr :: a }
    deriving (Eq, Show, Generic)
-}

class GIsElement a where
    gToElement :: a x -> XML.Element

instance ( KnownSymbol n
         , GAttrs c
         , GNodes c
         ) => GIsElement (D1 ('MetaData n x y z) c) where
    gToElement (M1 c) = XML.Element name attrs nodes
      where
        name  = symbolName (Proxy :: Proxy n)
        attrs = ggetAttrs c
        nodes = ggetNodes c

instance IsElement a => IsNode a where
    toNode = XML.NodeElement . toElement
    {-
    fromNode e n = case n of
        XML.NodeElement e' -> fromElement e'
        _                  -> failNodeType e NTElement
    -}

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
