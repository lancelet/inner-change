{-# LANGUAGE TemplateHaskell #-}

module Network.InnerChange.XML.TH where

import Data.Char (toUpper)
import qualified Data.Text as T (drop, cons, head, tail)
import Network.InnerChange.XML.Types (ToElement, FromElement,
                                      toElement, fromElement, genericEncConv,
                                      Options(Options), toNameSimple)

import Language.Haskell.TH

encodeElement :: Name -> Name -> Q [Dec]
encodeElement name encodingName = runQ
    [d|
        instance ToElement $(conT name) where
            toElement = toElement . f
              where
                f :: $(conT name) -> $(conT encodingName)
                f = genericEncConv
        instance FromElement $(conT name) where
            fromElement = fmap f . fromElement
              where
                f :: $(conT encodingName) -> $(conT name)
                f = genericEncConv
    |]

deriveToFromSoapElement :: Name -> Q [Dec]
deriveToFromSoapElement name = runQ
    [d|
        instance ToElement $(conT name) where
            toElement = genericToElement
                      $ Options
                        (toNameSimple . (T.drop 4))
                        (\t -> toNameSimple
                             $ T.cons (toUpper (T.head t)) (T.tail t))
                        id
        instance FromElement $(conT name) where
            fromElement = genericFromElement 
                        $ Options
                          (toNameSimple . (T.drop 4))
                          (\t -> toNameSimple
                               $ T.cons (toUpper (T.head t)) (T.tail t))
                          id
    |]
