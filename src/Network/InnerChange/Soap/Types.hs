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

newtype ElementName = ElementName XML.Name deriving (Eq, Show)
newtype AttributeName = AttributeName XML.Name deriving (Eq, Show)

data FailDetails
    = FailMissingElement ElementName
    | FailNotUniqueElement ElementName
    | FailAttributeMissing AttributeName
    | FailAttributeParse ElementName (AttributeName, Text)
    | FailTextMissing ElementName
    | FailTextParse ElementName
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

class IsText t where
    toText :: t -> Text
    fromText :: Text -> Maybe t

class IsElement a where
    classElementName :: Proxy a -> ElementName
    toElement :: a -> XML.Element
    fromElement :: XML.Element -> Result a

instance IsText Text where
    toText = id
    fromText = Just . id

-------------------------------------------------------------------------------

