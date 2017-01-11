{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ExplicitForAll        #-}

module Network.InnerChange.Soap.Types where

import qualified Data.Map.Strict            as Map (empty, fromList, insert)
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Text                  (Text)
import qualified Data.Text                  as Text (pack)
import qualified GHC.Generics               as G
import qualified Test.QuickCheck            as QC (Gen, elements)
import qualified Text.XML                   as XML (Element (Element),
                                                    Name (Name),
                                                    elementAttributes,
                                                    elementName)

-------------------------------------------------------------------------------

data FailDetails
    = FailElement XML.Name XML.Element
    | FailAttribute XML.Name (XML.Name, Text)
    | FailOther Text
    deriving (Eq, Show)

data Result a
    = Success a
    | Failure FailDetails
    deriving (Eq, Show, Functor)

instance Applicative Result where
    pure = Success
    Failure f <*> _         = Failure f
    _         <*> Failure f = Failure f
    Success f <*> Success x = Success (f x)

instance Monad Result where
    Failure f >>= _ = Failure f
    Success x >>= f = f x

-------------------------------------------------------------------------------

class ElementSerialization a where
    toElement   :: a -> XML.Element
    fromElement :: XML.Element -> Result a

-------------------------------------------------------------------------------

elementsGenGen :: forall a. (G.Generic a) => Proxy a -> QC.Gen a
elementsGenGen proxy = undefined
  where
    x = G.from proxy
