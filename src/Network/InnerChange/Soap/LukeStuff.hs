{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Network.InnerChange.Soap.LukeStuff where

import Data.Maybe
import qualified Text.XML as XML
import GHC.TypeLits
import Data.Proxy
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.IO as LText

-- Does this exist already?
class IsAttr t where
    toAttr :: t -> Text
    fromAttr :: Text -> Maybe t

instance IsAttr String where
    toAttr = Text.pack
    fromAttr = Just . Text.unpack

instance IsAttr Int where
    toAttr = Text.pack . show
    fromAttr = Just . read . Text.unpack -- *cough*

instance IsAttr Text where
    toAttr = id
    fromAttr = Just

data Attr (name :: Symbol) (t :: *)
data Child (t :: *)
data InElement (t :: Symbol) = InElement XML.Element

data (a :> b)

infixr 1 :>

type F = Attr "x" Int :> InElement "F"
type E = Attr "a" String :> Attr "b" Int :> Child F :> InElement "E"

class AsElement a where
    type MkElement a
    toElement :: Proxy a -> (Map XML.Name Text, [XML.Node]) -> MkElement a

instance KnownSymbol n => AsElement (InElement n) where
    type MkElement (InElement n) = XML.Element
    toElement _ (attrs,nodes) = XML.Element name attrs nodes
        where
            name = XML.Name (Text.pack $ symbolVal (Proxy :: Proxy n)) Nothing Nothing

instance (KnownSymbol n, IsAttr t, AsElement rest) => AsElement (Attr n t :> rest) where
    type MkElement (Attr n t :> rest) = t -> MkElement rest
    toElement _ (attrs, nodes) v = toElement (Proxy :: Proxy rest) (attrs', nodes)
        where
            name = XML.Name (Text.pack $ symbolVal (Proxy :: Proxy n)) Nothing Nothing
            attrs' = Map.insert name (toAttr v) attrs

-- Children are poorly handled for now.
instance (AsElement child, AsElement rest) => AsElement (Child child :> rest) where
    type MkElement (Child t :> rest) = XML.Element -> MkElement rest
    toElement _ (attrs, nodes) v = toElement (Proxy :: Proxy rest) (attrs, nodes')
        where
            nodes' = XML.NodeElement v : nodes

asElement :: AsElement a => Proxy a -> MkElement a
asElement proxy = toElement proxy (mempty, mempty)


class WithElement a where
    type FromElement a r
    withElement :: Proxy a -> FromElement a r -> XML.Element -> Maybe r

instance WithElement (InElement n) where
    type FromElement (InElement n) r = XML.Name -> r
    withElement _ f e = Just $ f name
        where
            name = XML.elementName e

instance (KnownSymbol n, IsAttr t, WithElement rest) => WithElement (Attr n t :> rest) where
    type FromElement (Attr n t :> rest) r = t -> FromElement rest r
    withElement _ f e = case v of
        Just v -> withElement (Proxy :: Proxy rest) (f v) e
        Nothing -> Nothing
        where
            v = do
                a <- Map.lookup name (XML.elementAttributes e)
                fromAttr a
            name = XML.Name (Text.pack $ symbolVal (Proxy :: Proxy n)) Nothing Nothing

-- Children are poorly handled for now
instance (WithElement child, WithElement rest) => WithElement (Child child :> rest) where
    type FromElement (Child child :> rest) r = XML.Element -> FromElement rest r
    withElement _ f e = case v of
        Just (v,e') -> withElement (Proxy :: Proxy rest) (f v) e'
        Nothing -> Nothing
        where
            v = case XML.elementNodes e of
                XML.NodeElement n:ns -> Just (n, e {XML.elementNodes = ns})
                _ -> Nothing

actionOnF :: FromElement F (IO ())
actionOnF x name = putStrLn $ "Element attribute value: " ++ show x

actionOnE :: FromElement E (IO ())
actionOnE a b child name = do
    -- Doesn't handle children very well yet
    fromJust $ withElement (Proxy :: Proxy F) actionOnF child
    print (a,b)

renderElement :: XML.Element -> LText.Text
renderElement e = XML.renderText XML.def (XML.Document (XML.Prologue [] Nothing []) e [])

main :: IO ()
main = do
    let
        myF = asElement (Proxy :: Proxy F) 5
    LText.putStrLn (renderElement myF)

    let
        myE = asElement (Proxy :: Proxy E) "Haha" 12 myF
    LText.putStrLn (renderElement myE)

    case withElement (Proxy :: Proxy E) actionOnE myE of
        Just io -> io
        Nothing -> putStrLn "Not a valid E"
