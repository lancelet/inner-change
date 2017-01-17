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

module Network.InnerChange.XML.Types
    ( -- * Classes
      ToElement (toElement)
    , FromElement (fromElement)
    , ToText (toText)
    , FromText (fromText)
    , ToNode (toNode)
    , FromNode (fromNode)
    , FromNodes' (fromNodesKnownOrder, fromNodesUnknownOrder)
    , ToAttrValue (toAttrValue)
    , FromAttrValue (fromAttrValue)
    , FromAttrValue' (fromAttrValue')
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
      --
    , Result' (Success', Failure')
    , RootFailure (FailExpectedElement, FailExpectedContent,
                   FailCouldNotParseAttribute, FailMissingAttribute,
                   FailMissingContent)
    , rootFail
    , maybeFail'
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

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------


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
