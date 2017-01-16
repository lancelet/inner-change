{-# OPTIONS_GHC -Wwarn #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Network.InnerChange.EWS.Types where

import           Network.InnerChange.XML.Types

import qualified Data.Char                     as Char (toUpper)
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text)
import qualified Data.Text                     as Text (cons, drop, head, tail)
import           GHC.Generics                  (Generic)
import qualified Text.XML                      as XML (Element, Name)


newtype Id = Id { unId :: Text }
             deriving (Eq, Show, Generic, ToText, FromText)


newtype ChangeKey = ChangeKey { unChangeKey :: Text }
                    deriving (Eq, Show, Generic, ToText, FromText)


-- https://msdn.microsoft.com/en-us/library/office/aa494315(v=exchg.150).aspx
data FieldURIFolderProperty
    = FFolderId
    | FParentFolderId
    | FDisplayName
    | FUnreadCount
    | FTotalCount
    | FChildFolderCount
    | FFolderClass
    | FSearchParameters
    | FManagedFolderInformation
    | FPermissionSet
    | FEffectiveRights
    | FSharingEffectiveRights
    deriving (Eq, Show, Generic)

fieldUriFolderPropertyOpts :: OptionsText
fieldUriFolderPropertyOpts = OptionsText $ \t -> "folder:" <> Text.drop 1 t

instance ToText FieldURIFolderProperty where
    toText = genericSumTypeToText fieldUriFolderPropertyOpts

instance FromText FieldURIFolderProperty where
    fromText = genericSumTypeFromText fieldUriFolderPropertyOpts


-------------------------------------------------------------------------------

-- https://msdn.microsoft.com/en-us/library/office/aa563810(v=exchg.150).aspx
data AdditionalProperties = AdditionalProperties [FieldURI]
                          deriving (Eq, Show, Generic)

-- https://msdn.microsoft.com/en-us/library/office/aa580545(v=exchg.150).aspx
data BaseShape = IdOnly
               | Default
               | AllProperties
               deriving (Eq, Show, Generic)

-- https://msdn.microsoft.com/en-us/library/office/aa580808(v=exchg.150).aspx
data DistinguishedFolder = Inbox
                         | Root
                         deriving (Eq, Show, Generic)

-- https://msdn.microsoft.com/en-us/library/office/aa580808(v=exchg.150).aspx
data DistinguishedFolderId = DistinguishedFolderId
    { dfId        :: DistinguishedFolder
    , dfChangeKey :: Maybe ChangeKey
    , dfMailbox   :: Mailbox
    } deriving (Eq, Show, Generic)

-- https://msdn.microsoft.com/en-us/library/office/aa565076(v=exchg.150).aspx
data EmailAddress = EmailAddress Text
                  deriving (Eq, Show, Generic)

-- https://msdn.microsoft.com/en-us/library/office/aa494315(v=exchg.150).aspx
data FieldURI = FieldURIFolder FieldURIFolderProperty
              deriving (Eq, Show, Generic)

-- https://msdn.microsoft.com/en-us/library/office/aa579461(v=exchg.150).aspx
data FolderId = FolderId Id (Maybe ChangeKey)
              deriving (Eq, Show, Generic)

-- https://msdn.microsoft.com/en-us/library/office/aa580509(v=exchg.150).aspx
data FolderIds = FolderIds [FolderId] [DistinguishedFolderId]
               deriving (Eq, Show, Generic)

-- https://msdn.microsoft.com/en-us/library/office/aa494311(v=exchg.150).aspx
data FolderShape = FolderShape BaseShape (Maybe AdditionalProperties)
                 deriving (Eq, Show, Generic)

-- https://msdn.microsoft.com/en-us/library/office/aa580234(v=exchg.150).aspx
data ItemId = ItemId Id (Maybe ChangeKey)
            deriving (Eq, Show, Generic)

-- https://msdn.microsoft.com/en-us/library/office/aa565036(v=exchg.150).aspx
data Mailbox = Mailbox
    { mbName         :: Maybe Name
    , mbEmailAddress :: Maybe EmailAddress
    , mbType         :: Maybe MailboxType
    , mbItemId       :: Maybe ItemId
    } deriving (Eq, Show, Generic)

-- https://msdn.microsoft.com/en-us/library/office/aa563493(v=exchg.150).aspx
data MailboxType = MMailbox
                 | MPublicDL
                 | MPrivateDL
                 | MPublicFolder
                 | MUnknown
                 | MOneOff
                 | MGroupMailbox
                 deriving (Eq, Show, Generic)

-- https://msdn.microsoft.com/en-us/library/office/aa564260(v=exchg.150).aspx
data Name = Name Text
          deriving (Eq, Show, Generic)

-- https://msdn.microsoft.com/en-us/library/office/aa494327(v=exchg.150).aspx
data ParentFolderId = ParentFolderId Id (Maybe ChangeKey)
                    deriving (Eq, Show, Generic)

-------------------------------------------------------------------------------

instance Encode AdditionalProperties where
    type EncodeFor AdditionalProperties = SoapAdditionalProperties
instance ToElement AdditionalProperties where
    toElement = toElement . encode
instance FromElement AdditionalProperties where
    fromElement = fmap decode . fromElement

instance ToElement BaseShape where
    toElement = genericSumTypeToElement sencopt
instance FromElement BaseShape where
    fromElement = genericSumTypeFromElement sencopt

instance ToText DistinguishedFolder where
    toText = genericSumTypeToText distinguishedFolderOpts
instance FromText DistinguishedFolder where
    fromText = genericSumTypeFromText distinguishedFolderOpts

instance ToAttrValue DistinguishedFolder where
    toAttrValue = Just . toText
instance FromAttrValue DistinguishedFolder where
    fromAttrValue = (=<<) fromText
distinguishedFolderOpts :: OptionsText
distinguishedFolderOpts = OptionsText $ Prelude.id

instance Encode DistinguishedFolderId where
    type EncodeFor DistinguishedFolderId = SoapDistinguishedFolderId
instance ToElement DistinguishedFolderId where
    toElement = toElement . encode
instance FromElement DistinguishedFolderId where
    fromElement = fmap decode . fromElement

instance Encode EmailAddress where
    type EncodeFor EmailAddress = SoapEmailAddress
instance ToElement EmailAddress where
    toElement = toElement . encode
instance FromElement EmailAddress where
    fromElement = fmap decode . fromElement
    
instance ToAttrValue FieldURI where
    toAttrValue (FieldURIFolder p) = Just $ toText p
instance FromAttrValue FieldURI where
    fromAttrValue Nothing = Nothing
    fromAttrValue (Just text) = FieldURIFolder <$> fromText text
instance ToElement FieldURI where
    toElement f = toElement $ SoapFieldURI (Attr f)
instance FromElement FieldURI where
    fromElement e = (\(SoapFieldURI (Attr f)) -> f)
                <$> fromElement e

instance Encode FolderId where
    type EncodeFor FolderId = SoapFolderId
instance ToElement FolderId where
    toElement = toElement . encode
instance FromElement FolderId where
    fromElement = fmap decode . fromElement
    
instance Encode FolderShape where
    type EncodeFor FolderShape = SoapFolderShape
instance ToElement FolderShape where
    toElement = toElement . encode
instance FromElement FolderShape where
    fromElement = fmap decode . fromElement

instance Encode ItemId where
    type EncodeFor ItemId = SoapItemId
instance ToElement ItemId where
    toElement = toElement . encode
instance FromElement ItemId where
    fromElement = fmap decode . fromElement

instance Encode Mailbox where
    type EncodeFor Mailbox = SoapMailbox
instance ToElement Mailbox where
    toElement = toElement . encode
instance FromElement Mailbox where
    fromElement = fmap decode . fromElement

instance ToElement MailboxType where
    toElement = genericSumTypeToElement mbEncopt
instance FromElement MailboxType where
    fromElement = genericSumTypeFromElement mbEncopt
mbEncopt :: Options
mbEncopt = Options (toNameSimple . (Text.drop 1)) toAttrName toCst

instance Encode Name where
    type EncodeFor Name = SoapName
instance ToElement Name where
    toElement = toElement . encode
instance FromElement Name where
    fromElement = fmap decode . fromElement


instance Encode ParentFolderId where
    type EncodeFor ParentFolderId = SoapParentFolderId
instance ToElement ParentFolderId where
    toElement = toElement . encode
instance FromElement ParentFolderId where
    fromElement = fmap decode . fromElement

-------------------------------------------------------------------------------

data SoapAdditionalProperties = SoapAdditionalProperties
    { fieldURIs :: [FieldURI]
    } deriving (Eq, Show, Generic)

instance ToElement SoapAdditionalProperties where
    toElement = genericToElement encopt

instance FromElement SoapAdditionalProperties where
    fromElement = genericFromElement encopt


data SoapDistinguishedFolderId = SoapDistinguishedFolderId
    { id        :: Attr DistinguishedFolder
    , changeKey :: Attr (Maybe ChangeKey)
    , mailbox   :: Mailbox
    } deriving (Eq, Show, Generic)

instance ToElement SoapDistinguishedFolderId where
    toElement = genericToElement encopt
    
instance FromElement SoapDistinguishedFolderId where
    fromElement = genericFromElement encopt


data SoapEmailAddress = SoapEmailAddress
    { emailAddress :: Text
    } deriving (Eq, Show, Generic)

instance ToElement SoapEmailAddress where
    toElement = genericToElement encopt

instance FromElement SoapEmailAddress where
    fromElement = genericFromElement encopt


data SoapFieldURI = SoapFieldURI
    { fieldURI :: Attr FieldURI
    } deriving (Eq, Show, Generic)

instance ToElement SoapFieldURI where
    toElement = genericToElement encopt

instance FromElement SoapFieldURI where
    fromElement = genericFromElement encopt


data SoapFolderId = SoapFolderId
    { id        :: Attr Id
    , changeKey :: Attr (Maybe ChangeKey)
    } deriving (Eq, Show, Generic)

instance ToElement SoapFolderId where
    toElement = genericToElement encopt

instance FromElement SoapFolderId where
    fromElement = genericFromElement encopt


data SoapFolderShape = SoapFolderShape
    { baseShape       :: BaseShape
    , additionalProps :: Maybe AdditionalProperties
    } deriving (Eq, Show, Generic)

instance ToElement SoapFolderShape where
    toElement = genericToElement encopt

instance FromElement SoapFolderShape where
    fromElement = genericFromElement encopt


data SoapItemId = SoapItemId
    { id        :: Attr Id
    , changeKey :: Attr (Maybe ChangeKey)
    } deriving (Eq, Show, Generic)

instance ToElement SoapItemId where
    toElement = genericToElement encopt

instance FromElement SoapItemId where
    fromElement = genericFromElement encopt


data SoapMailbox = SoapMailbox
    { smName         :: Maybe Name
    , smEmailAddress :: Maybe EmailAddress
    , smType         :: Maybe MailboxType
    , smItemId       :: Maybe ItemId
    } deriving (Eq, Show, Generic)

instance ToElement SoapMailbox where
    toElement = genericToElement encopt

instance FromElement SoapMailbox where
    fromElement = genericFromElement encopt


data SoapName = SoapName
    { name :: Text }
    deriving (Eq, Show, Generic)

instance ToElement SoapName where
    toElement = genericToElement encopt

instance FromElement SoapName where
    fromElement = genericFromElement encopt


data SoapParentFolderId = SoapParentFolderId
    { id        :: Attr Id
    , changeKey :: Attr (Maybe ChangeKey)
    } deriving (Eq, Show, Generic)

instance ToElement SoapParentFolderId where
    toElement = genericToElement encopt

instance FromElement SoapParentFolderId where
    fromElement = genericFromElement encopt


-------------------------------------------------------------------------------

encopt :: Options
encopt = Options toElName toAttrName toCst

sencopt :: Options
sencopt = Options toNameSimple toAttrName toCst

toElName :: Text -> XML.Name
toElName = toNameSimple . (Text.drop 4)

toAttrName :: Text -> XML.Name
toAttrName = toNameSimple . capitalizeFirst
  where
    capitalizeFirst t = Text.cons (Char.toUpper (Text.head t)) (Text.tail t)

toCst :: Text -> Text
toCst = Prelude.id

-------------------------------------------------------------------------------

-- temporary test stuff

testItemId1, testItemId2 :: ItemId
testItemId1 = ItemId (Id "ABBA") Nothing
testItemId2 = ItemId (Id "BABBA") (Just (ChangeKey "CAFEBABE"))

e1, e2 :: XML.Element
e1 = toElement testItemId1
e2 = toElement testItemId2

read1, read2 :: Result ItemId
read1 = fromElement e1
read2 = fromElement e2
