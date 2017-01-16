-- {-# OPTIONS_GHC -Wwarn #-}
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

module Network.InnerChange.XML.Types
    ( -- * Classes
      ToElement (toElement)
    , FromElement (fromElement)
    , ToText (toText)
    , FromText (fromText)
    , ToNode (toNode)
    , FromNode (fromNode)
    , ToAttrValue (toAttrValue)
    , FromAttrValue (fromAttrValue)
    , Conv (conv)
    , Encode (encode, decode)
    , EncodeFor
      -- * Types
    , Attr (Attr, unAttr)
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
    , genericSumTypeToElement
    , genericSumTypeFromElement
    , genericSumTypeToText
    , genericSumTypeFromText
    , genericEncConv
      -- * Options for generics
    , Options (Options, toElementName, toAttributeName, toContentSumType)
    , defaultOptions
    , toNameSimple
    , OptionsText (OptionsText, constructorToText)
    , defaultOptionsText
      -- * Helpers
    , toTextToNode
    , fromTextFromNode
    , showText
    , readMaybeText
    ) where

import           Control.Applicative (Alternative, empty, (<|>))
import           Data.Map            (Map)
import qualified Data.Map            as Map (lookup, singleton, union)
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
    toElement = genericToElement defaultOptions

class FromElement a where
    fromElement :: XML.Element -> Result a

    default fromElement :: (Generic a, GFromElement (Rep a))
                        => XML.Element
                        -> Result a
    fromElement = genericFromElement defaultOptions

class ToText a where
    toText :: a -> Text

class FromText a where
    fromText :: Text -> Maybe a

class ToNode a where
    toNode :: a -> [XML.Node]

class FromNode a where
    fromNode :: [XML.Node] -> (Result a, [XML.Node])

-------------------------------------------------------------------------------

showText :: (Show a) => a -> Text
showText = Text.pack . show

readMaybeText :: (Read a) => Text -> Maybe a
readMaybeText = Read.readMaybe . Text.unpack

instance ToText Text where toText = id
instance FromText Text where fromText = Just . id

instance ToText Int where toText = showText
instance FromText Int where fromText = readMaybeText

-------------------------------------------------------------------------------

newtype Attr a = Attr { unAttr :: a } deriving (Eq, Show, Generic)

data Options = Options
    { toElementName    :: Text -> XML.Name
    , toAttributeName  :: Text -> XML.Name
    , toContentSumType :: Text -> Text }

defaultOptions :: Options
defaultOptions = Options toNameSimple toNameSimple id

toNameSimple :: Text -> XML.Name
toNameSimple t = XML.Name t Nothing Nothing

symbolText :: (KnownSymbol s) => Proxy s -> Text
symbolText = Text.pack . symbolVal

data OptionsText = OptionsText
    { constructorToText :: Text -> Text }

defaultOptionsText :: OptionsText
defaultOptionsText = OptionsText id

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

instance Alternative Result where
    empty = Failure $ FailureAlternativeEmpty
    Success v <|> _         = Success v
    Failure _ <|> Success v = Success v
    Failure _ <|> Failure f = Failure f

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
    | FailureAlternativeEmpty
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
        Just x  -> (Success x, ns')
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

partialElementName :: XML.Name -> (PartialElement -> PartialElement)
partialElementName name initPe = initPe { peName = Just name }

addAttribute :: (ToAttrValue a)
             => XML.Name
             -> a
             -> (PartialElement -> PartialElement)
addAttribute name x initPe = case toAttrValue x of
    Just v -> initPe { peAttrs = Map.union (peAttrs initPe) attr }
      where
        attr = Map.singleton name v
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

genericToElement :: (Generic a, GToElement (Rep a))
                 => Options
                 -> a
                 -> XML.Element
genericToElement opts x = freeze $ gToElement opts (from x) emptyPartialElement

class GToElement a where
    gToElement :: Options -> a x -> (PartialElement -> PartialElement)

instance ( KnownSymbol name
         , GToElement c
         ) => GToElement (D1 ('MetaData name x y z) c) where
    gToElement opts (M1 c)
        = (gToElement opts c) . (partialElementName elementName)
      where
        elementName = toElementName opts (symbolText (Proxy :: Proxy name))

instance GToElement s => GToElement (C1 ('MetaCons name q w) s) where
    gToElement opts (M1 sel)
        = gToElement opts sel

instance {-# OVERLAPS #-}
         ( KnownSymbol name
         , ToAttrValue a
         ) => GToElement (S1 ('MetaSel ('Just name) q w e)
                             (Rec0 (Attr a))) where
    gToElement opts (M1 (K1 (Attr x)))
        = addAttribute attributeName x
      where
        attributeName = toAttributeName opts (symbolText (Proxy :: Proxy name))

instance ToNode a
         => GToElement (S1 ('MetaSel ('Just name) q w e) (Rec0 a)) where
    gToElement _ (M1 (K1 x)) = addNodes x

instance ( GToElement a
         , GToElement b
         ) => GToElement (a :*: b) where
    gToElement opts (a :*: b) = (gToElement opts b) . (gToElement opts a)

-------------------------------------------------------------------------------

genericFromElement :: (Generic a, GFromElement (Rep a))
                   => Options
                   -> XML.Element
                   -> Result a
genericFromElement opts e = (to . fst) <$> gFromElement opts (toPartial e)

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

class GFromElement a where
    gFromElement :: Options -> PartialElement -> Result (a x, PartialElement)

instance ( KnownSymbol name
         , GFromElement c
         ) => GFromElement (D1 ('MetaData name x y z) c) where
    gFromElement opts
        = fmap (mapFst M1)
        . whenElement elementName (gFromElement opts)
      where
        elementName = toElementName opts (symbolText (Proxy :: Proxy name))

instance GFromElement s
         => GFromElement (C1 ('MetaCons name q w) s) where
    gFromElement opts = fmap (mapFst M1) . (gFromElement opts)

instance {-# OVERLAPS #-}
         ( KnownSymbol name
         , FromAttrValue a
         ) => GFromElement (S1 ('MetaSel ('Just name) q w e)
                               (Rec0 (Attr a))) where
    gFromElement opts
        = fmap ((mapFst M1) . (mapFst K1) . (mapFst Attr))
        . takeAttribute attributeName
      where
        attributeName = toAttributeName opts (symbolText (Proxy :: Proxy name))

instance FromNode a
         => GFromElement (S1 ('MetaSel ('Just name) q w e) (Rec0 a)) where
    gFromElement _ = fmap ((mapFst M1) . (mapFst K1)) . takeNodes

instance ( GFromElement a
         , GFromElement b
         ) => GFromElement (a :*: b) where
    gFromElement opts e = do
        (l, e')  <- gFromElement opts e
        (r, e'') <- gFromElement opts e'
        Success (l :*: r, e'')

-------------------------------------------------------------------------------

genericSumTypeToElement :: ( Generic a
                           , GSumTypeToElement (Rep a) )
                        => Options
                        -> a
                        -> XML.Element
genericSumTypeToElement opts x
    = freeze $ gsumTypeToElement opts (from x) emptyPartialElement

class GSumTypeToElement a where
    gsumTypeToElement :: Options -> a r -> (PartialElement -> PartialElement)

instance ( KnownSymbol name
         , GSumTypeToElement c
         ) => GSumTypeToElement (D1 ('MetaData name x y z) c) where
    gsumTypeToElement opts (M1 c)
        = (gsumTypeToElement opts c) . (partialElementName elementName)
      where
        elementName = toElementName opts (symbolText (Proxy :: Proxy name))

instance ( KnownSymbol name
         ) => GSumTypeToElement (C1 ('MetaCons name x y) U1) where
    gsumTypeToElement opts _ = addNodes content
      where
        content = toContentSumType opts (symbolText (Proxy :: Proxy name))

instance ( GSumTypeToElement a
         , GSumTypeToElement b
         ) => GSumTypeToElement (a :+: b) where
    gsumTypeToElement opts (L1 x) = gsumTypeToElement opts x
    gsumTypeToElement opts (R1 x) = gsumTypeToElement opts x

-------------------------------------------------------------------------------

genericSumTypeFromElement :: ( Generic a
                             , GSumTypeFromElement (Rep a) )
                          => Options
                          -> XML.Element
                          -> Result a
genericSumTypeFromElement opts e
    = (to . fst) <$> gsumTypeFromElement opts (toPartial e)



class GSumTypeFromElement a where
    gsumTypeFromElement :: Options
                        -> PartialElement
                        -> Result (a x, PartialElement)

instance ( KnownSymbol name
         , GSumTypeFromElement c
         ) => GSumTypeFromElement (D1 ('MetaData name x y z) c) where
    gsumTypeFromElement opts
        = fmap (mapFst M1)
        . whenElement elementName (gsumTypeFromElement opts)
      where
        elementName = toElementName opts (symbolText (Proxy :: Proxy name))

instance ( KnownSymbol name
         ) => GSumTypeFromElement (C1 ('MetaCons name x y) U1) where
    gsumTypeFromElement opts pe
        = takeNodes pe
      >>= \(t, _) -> if t == content
                     then Success (M1 U1, pe)
                     else Failure $ FailureParseContent t
      where
        content = toContentSumType opts (symbolText (Proxy :: Proxy name))

instance ( GSumTypeFromElement a
         , GSumTypeFromElement b
         ) => GSumTypeFromElement (a :+: b) where
    gsumTypeFromElement opts pe
        =   ((mapFst L1) <$> gsumTypeFromElement opts pe)
        <|> ((mapFst R1) <$> gsumTypeFromElement opts pe)

-------------------------------------------------------------------------------

genericSumTypeToText :: ( Generic a
                        , GSumTypeToText (Rep a) )
                     => OptionsText
                     -> a
                     -> Text
genericSumTypeToText opts = (gsumTypeToText opts) . from

class GSumTypeToText a where
    gsumTypeToText :: OptionsText -> a r -> Text

instance GSumTypeToText c => GSumTypeToText (D1 z c) where
    gsumTypeToText opts (M1 x) = gsumTypeToText opts x

instance (KnownSymbol name) => GSumTypeToText (C1 ('MetaCons name x y) U1) where
    gsumTypeToText opts _
        = constructorToText opts (symbolText (Proxy :: Proxy name))

instance (GSumTypeToText a, GSumTypeToText b) => GSumTypeToText (a :+: b) where
    gsumTypeToText opts (L1 x) = gsumTypeToText opts x
    gsumTypeToText opts (R1 x) = gsumTypeToText opts x

-------------------------------------------------------------------------------

genericSumTypeFromText :: ( Generic a
                          , GSumTypeFromText (Rep a) )
                       => OptionsText
                       -> Text
                       -> Maybe a
genericSumTypeFromText opts text = to <$> gsumTypeFromText opts text

class GSumTypeFromText a where
    gsumTypeFromText :: OptionsText -> Text -> Maybe (a r)

instance GSumTypeFromText c => GSumTypeFromText (D1 z c) where
    gsumTypeFromText opts = (fmap M1) . (gsumTypeFromText opts)

instance ( KnownSymbol name
         ) => GSumTypeFromText (C1 ('MetaCons name x y) U1) where
    gsumTypeFromText opts text
        = if text == cName
          then Just (M1 U1)
          else Nothing
      where
        cName = constructorToText opts (symbolText (Proxy :: Proxy name))

instance ( GSumTypeFromText a
         , GSumTypeFromText b
         ) => GSumTypeFromText (a :+: b) where
    gsumTypeFromText opts text
        =   (L1 <$> gsumTypeFromText opts text)
        <|> (R1 <$> gsumTypeFromText opts text)

-------------------------------------------------------------------------------

class Conv a b where
    conv :: a -> b

    default conv :: (Generic a, Generic b, GEncConv (Rep a) (Rep b))
                 => a -> b
    conv = genericEncConv

instance Conv a a where
    conv = id

instance Conv a (Attr a) where
    conv = Attr

instance Conv (Attr a) a where
    conv = unAttr

genericEncConv :: ( Generic a
                  , Generic b
                  , GEncConv (Rep a) (Rep b) )
               => a -> b
genericEncConv xa = to $ gencConv $ from xa

class GEncConv a b where
    gencConv :: a r -> b r

instance Conv a b => GEncConv (K1 i a) (K1 j b) where
    gencConv = K1 . conv . unK1

instance GEncConv a b => GEncConv (M1 i c a) (M1 j d b) where
    gencConv = M1 . gencConv . unM1

instance GEncConv V1 V1 where
    gencConv = id

instance ( GEncConv f1 f2
         , GEncConv g1 g2
         ) => GEncConv (f1 :*: g1) (f2 :*: g2) where
    gencConv (l :*: r) = gencConv l :*: gencConv r

-------------------------------------------------------------------------------
    
class Encode a where
    type EncodeFor a
    encode :: a -> EncodeFor a
    decode :: EncodeFor a -> a

    default encode :: ( Generic a
                      , Generic (EncodeFor a)
                      , GEncConv (Rep a) (Rep (EncodeFor a)) )
                   => a -> (EncodeFor a)
    encode = genericEncConv

    default decode :: ( Generic a
                      , Generic (EncodeFor a)
                      , GEncConv (Rep (EncodeFor a)) (Rep a) )
                   => (EncodeFor a) -> a
    decode = genericEncConv
