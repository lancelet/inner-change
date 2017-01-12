{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Network.InnerChange.Soap.Luke where

import           Network.InnerChange.Soap.Types (AttributeName (AttributeName),
                                                 ElementName (ElementName),
                                                 FailDetails (FailAttributeMissing, FailAttributeParse, FailMissingElement, FailNotUniqueElement, FailOther, FailTextParse),
                                                 IsElement, IsText,
                                                 Result (Failure, Success),
                                                 fromElement, fromText,
                                                 toElement, toText,
                                                 classElementName)

import           Data.Map                       (Map)
import qualified Data.Map                       as Map (insert, lookup)
import           Data.Maybe                     (mapMaybe)
import           Data.Proxy                     (Proxy (Proxy))
import           Data.Text                      (Text)
import qualified Data.Text                      as Text (pack)
import           GHC.TypeLits                   (KnownSymbol, Symbol, symbolVal)
import           Text.XML                       as XML (Element (Element),
                                                        Name (Name),
                                                        Node (NodeContent, NodeElement),
                                                        elementAttributes,
                                                        elementName,
                                                        elementNodes)

-------------------------------------------------------------------------------

txtSymbol :: (KnownSymbol s) => Proxy s -> Text
txtSymbol = Text.pack . symbolVal

symName :: (KnownSymbol s) => Proxy s -> XML.Name
symName p = XML.Name (txtSymbol p) Nothing Nothing

-------------------------------------------------------------------------------

data Attr (name :: Symbol) (t :: *)
data Child (t :: *)
data Content (t :: *)
data InElement (name :: Symbol) = InElement XML.Element

data (a :> b)
infixr 1 :>

type F = Attr "x" Text :> InElement "F"

data FType = FType Text
instance IsElement FType where
    classElementName _ = ElementName $ "F"
    fromElement = const $ Failure $ FailOther "Not Implemented"
    toElement (FType x) = packElement (Proxy :: Proxy F) x

type E = Attr "a" Text :> Attr "b" Text :> Child FType :> Content Text :> InElement "E"
data EType = EType Text Text FType Text

-------------------------------------------------------------------------------

class AsElement a where
    type MkElement a
    mkElement :: Proxy a
              -> (Map XML.Name Text, [XML.Node])
              -> MkElement a

instance KnownSymbol n => AsElement (InElement n) where
    type MkElement (InElement n) = XML.Element
    mkElement _ (attrs, nodes)
        = XML.Element (symName (Proxy :: Proxy n)) attrs nodes

instance ( KnownSymbol n
         , IsText t
         , AsElement r ) => AsElement (Attr n t :> r) where
    type MkElement (Attr n t :> r) = t -> MkElement r
    mkElement _ (attrs, nodes) v = mkElement (Proxy :: Proxy r) (attrs', nodes)
      where
        name   = symName (Proxy :: Proxy n)
        attrs' = Map.insert name (toText v) attrs

instance (IsElement c, AsElement r) => AsElement (Child c :> r) where
    type MkElement (Child c :> r) = c -> MkElement r
    mkElement _ (attrs, nodes) v = mkElement (Proxy :: Proxy r) (attrs, nodes')
      where
        nodes' = XML.NodeElement (toElement v) : nodes

instance (IsText t, AsElement r) => AsElement (Content t :> r) where
    type MkElement (Content t :> r) = t -> MkElement r
    mkElement _ (attrs, nodes) v = mkElement (Proxy :: Proxy r) (attrs, nodes')
      where
        nodes' = XML.NodeContent (toText v) : nodes

packElement :: AsElement a => Proxy a -> MkElement a
packElement proxy = mkElement proxy (mempty, mempty)

-------------------------------------------------------------------------------

class WithElement a where
    type UnElement a r
    withElement :: Proxy a -> UnElement a r -> XML.Element -> Result r

instance WithElement (InElement n) where
    type UnElement (InElement n) r = XML.Name -> r
    withElement _ f e = Success $ f (XML.elementName e)

instance ( KnownSymbol n
         , IsText t
         , WithElement rest ) => WithElement (Attr n t :> rest) where
    type UnElement (Attr n t :> rest) r = t -> UnElement rest r
    withElement _ f e = lookupAttribute e name >>= continue
      where
        name = symName (Proxy :: Proxy n)
        continue v = withElement (Proxy :: Proxy rest) (f v) e

lookupTextAttribute :: XML.Element -> XML.Name -> Result Text
lookupTextAttribute e name = case Map.lookup name (XML.elementAttributes e) of
    Nothing   -> Failure $ FailAttributeMissing $ AttributeName name
    Just text -> Success text

lookupAttribute :: (IsText t) => XML.Element -> XML.Name -> Result t
lookupAttribute e name = lookupTextAttribute e name >>= \text ->
    case fromText text of
        Nothing    -> Failure $ FailAttributeParse elName (atName, text)
        Just value -> Success value
  where
    elName = ElementName $ XML.elementName e
    atName = AttributeName name

instance ( IsElement c
         , WithElement rest
         ) => WithElement (Child c :> rest) where
    type UnElement (Child c :> rest) r = c -> UnElement rest r
    withElement _ f e = lookupChildElement e name >>= fromElement >>= continue
      where
        name = case classElementName (Proxy :: Proxy c) of ElementName n -> n
        continue v = withElement (Proxy :: Proxy rest) (f v) e

childElementsWithName :: XML.Element -> XML.Name -> [XML.Element]
childElementsWithName e name = mapMaybe getEls (XML.elementNodes e)
  where
    getEls node = case node of
                      XML.NodeElement e@(XML.Element n _ _) ->
                          if n == name
                          then Just e
                          else Nothing
                      _ -> Nothing

lookupChildElement :: XML.Element -> XML.Name -> Result XML.Element
lookupChildElement e name = case childElementsWithName e name of
    [x] -> Success x
    []  -> Failure $ FailMissingElement   $ ElementName name
    _   -> Failure $ FailNotUniqueElement $ ElementName name

instance ( IsText t
         , WithElement rest
         ) => WithElement (Content t :> rest) where
    type UnElement (Content t :> rest) r = t -> UnElement rest r
    withElement _ f e = fromTextResult elName (textContent e) >>= continue
      where
        elName = ElementName $ XML.elementName e
        continue v = withElement (Proxy :: Proxy rest) (f v) e

fromTextResult :: (IsText t) => ElementName -> Text -> Result t
fromTextResult elName text = case fromText text of
    Just value -> Success value
    Nothing    -> Failure $ FailTextParse elName

textContent :: XML.Element -> Text
textContent e = mconcat $ mapMaybe getEls (XML.elementNodes e)
  where
    getEls node = case node of
                      XML.NodeContent text -> Just text
                      _                    -> Nothing

-------------------------------------------------------------------------------

-- some tests...

{-
f :: XML.Element
f = packElement (Proxy :: Proxy F) "x value"
-}
ftype :: FType
ftype = FType "x value"

e :: XML.Element
e = packElement (Proxy :: Proxy E) "a value" "b value" ftype "Contents"
