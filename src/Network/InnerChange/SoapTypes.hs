{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Network.InnerChange.SoapTypes where

import qualified Crypto.Hash     as Hash (Digest, SHA1, hash)
import qualified Data.ByteString.UTF8 as BS (fromString)
import qualified Data.Map.Strict as Map (empty, singleton, fromList, lookup)
import           Data.Maybe      (catMaybes, mapMaybe)
import           Data.Monoid     ((<>))
import           Data.String     (IsString, fromString)
import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.Lazy  as LT (toStrict)
import qualified Test.QuickCheck as QC (Arbitrary, arbitrary, elements)
import qualified Text.XML        as XML (Document (Document), Element (Element),
                                         Name, Node (NodeContent, NodeElement),
                                         Prologue (Prologue), def, renderText,
                                         elementName, elementAttributes,
                                         elementNodes)


-- $setup
-- DocTest setup:
-- >>> :set -XOverloadedStrings

-------------------------------------------------------------------------------

-- | Renders an XML Element as a Text string.
--
-- >>> elemToText $ XML.Element "Element" Map.empty []
-- "<Element/>"
elemToText :: XML.Element -> Text
elemToText e = snd
             $ T.breakOnEnd "?>"  -- drop the XML prolog
             $ LT.toStrict
             $ XML.renderText XML.def doc
  where
    doc = XML.Document prologue e []
    prologue = XML.Prologue [] Nothing []

-- | Creates an XML Element with a given name and content.
--
-- The XML element has no attributes.
--
-- >>> elemToText $ txtElement "Foo" "Bar"
-- "<Foo>Bar</Foo>"
txtElement :: XML.Name -> Text -> XML.Element
txtElement name content = XML.Element name Map.empty [ XML.NodeContent content ]

-------------------------------------------------------------------------------

hashInt :: Int -> Hash.Digest Hash.SHA1
hashInt = Hash.hash . BS.fromString . show

hashToText :: Hash.Digest Hash.SHA1 -> Text
hashToText = T.pack . show

-------------------------------------------------------------------------------

-- | Serialization of some type 'a' to an 'XML.Element'.
class ToElement a where
    toElement :: a -> XML.Element

-- | De-serialization of an 'XML.Element' to a type 'a'.
class FromElement a where
    fromElement :: XML.Element -> FEResult a

-- | Result of de-serializing some type 'a' from an 'XML.Element'.
data FEResult a = FESuccess a
                | FEFailure Text
                deriving (Eq, Show, Functor)

instance Applicative FEResult where
    pure x = FESuccess x
    (FEFailure t) <*> _ = FEFailure t
    (FESuccess f) <*> (FEFailure t) = FEFailure t
    (FESuccess f) <*> (FESuccess x) = FESuccess (f x)
    
instance Monad FEResult where
    (FEFailure t) >>= _ = FEFailure t
    (FESuccess x) >>= f = f x

failElement :: XML.Name -> XML.Element -> FEResult a
failElement name e =
    FEFailure $ "Could not parse " <> eltxt <> " as a " <> nametxt
  where
    eltxt = elemToText e
    nametxt = T.pack $ show $ name

getAttr :: XML.Name -> XML.Element -> FEResult Text
getAttr name e = case Map.lookup name (XML.elementAttributes e) of
    Just text -> FESuccess text
    Nothing   -> FEFailure
               $ "Could not find required attribute "
              <> (T.pack . show) name
              <> " in element "
              <> (T.pack . show) (XML.elementName e)

getOptionalAttr :: XML.Name -> XML.Element -> FEResult (Maybe Text)
getOptionalAttr name e = case Map.lookup name (XML.elementAttributes e) of
    Just text -> (FESuccess . Just) text
    Nothing   -> FESuccess Nothing

-- | Returns a successful result containing child elements of the specified
-- name, only if all child elements have the given name.
acceptOnlyChildrenWithName :: XML.Name -> XML.Element -> FEResult [XML.Element]
acceptOnlyChildrenWithName name e =
    if allMatch
    then FESuccess childElements
    else FEFailure $ "Found the following unexpected elements: ["
                  <> T.intercalate ", " nonMatching
                  <> "]; only "
                  <> (T.pack . show) name
                  <> " elements are expected."
  where
    nameMatches e = name == XML.elementName e
    allMatch = all nameMatches childElements
    nonMatching = (T.pack . show)
              <$> XML.elementName
              <$> filter (not . nameMatches) childElements 
    childElements = mapMaybe getElement (XML.elementNodes e)
    getElement n = case n of
        XML.NodeElement e -> Just e
        _                 -> Nothing

-------------------------------------------------------------------------------

newtype Id = Id Text deriving (Eq, Show)

instance IsString Id where
    fromString = Id . fromString

instance QC.Arbitrary Id where
    arbitrary = Id <$> (hashToText . hashInt) <$> QC.arbitrary

idText :: Id -> Text
idText (Id x) = x

getIdAttr :: XML.Element -> FEResult Id
getIdAttr e = Id <$> getAttr "Id" e

newtype ChangeKey = ChangeKey Text deriving (Eq, Show)

instance IsString ChangeKey where
    fromString = ChangeKey . fromString

instance QC.Arbitrary ChangeKey where
    arbitrary = ChangeKey <$> (hashToText . hashInt) <$> QC.arbitrary

changeKeyText :: ChangeKey -> Text
changeKeyText (ChangeKey k) = k

getChangeKeyOptionalAttr :: XML.Element -> FEResult (Maybe ChangeKey)
getChangeKeyOptionalAttr e = fmap ChangeKey <$> getOptionalAttr "ChangeKey" e
    
-------------------------------------------------------------------------------

-- | Unique identifier and change key of an item in the Exchange store.
--
-- https://msdn.microsoft.com/en-us/library/office/aa580234(v=exchg.150).aspx
--
-- >>> elemToText . toElement $ ItemId "42" (Just "CA00842")
-- "<ItemId ChangeKey=\"CA00842\" Id=\"42\"/>"
--
-- prop> \i -> FESuccess i == (fromElement . toElement) (i :: ItemId)
data ItemId = ItemId Id (Maybe ChangeKey) deriving (Eq, Show)

instance ToElement ItemId where
    toElement (ItemId id km) = XML.Element "ItemId" attrs []
      where
        attrs = Map.fromList
              $ catMaybes
              [ Just ("Id", idText id)
              , (\ck -> ("ChangeKey", changeKeyText ck)) <$> km ]

instance FromElement ItemId where
    fromElement e@(XML.Element "ItemId" attrs []) = 
        ItemId <$> getIdAttr e
               <*> getChangeKeyOptionalAttr e
    fromElement e = failElement "ItemId" e

instance QC.Arbitrary ItemId where
    arbitrary = ItemId <$> QC.arbitrary <*> QC.arbitrary

-------------------------------------------------------------------------------

-- | FieldURI
--
-- https://msdn.microsoft.com/en-us/library/office/aa494315(v=exchg.150).aspx
--
-- TODO: extend with properties for item, message, etc.
--
-- >>> elemToText . toElement $ FieldURIFolder FPFolderId
-- "<FieldURI FieldURI=\"folder:FolderId\"/>"
--
-- prop> \i -> FESuccess i == (fromElement . toElement) (i :: FieldURI)
data FieldURI
    = FieldURIFolder FolderProperty
    deriving (Eq, Show)

instance ToElement FieldURI where
    toElement x = XML.Element "FieldURI" attr []
      where
        attr = Map.singleton "FieldURI"
             $ case x of
                   FieldURIFolder p -> folderPropertyToText p

instance FromElement FieldURI where
    fromElement e@(XML.Element "FieldURI" attrs []) =
        FieldURIFolder <$> (getAttr "FieldURI" e >>= parseFolderProperty)

instance QC.Arbitrary FieldURI where
    arbitrary = FieldURIFolder <$> QC.arbitrary

-- | Folder property subset of 'FieldURI'.
--
-- https://msdn.microsoft.com/en-us/library/office/aa494315(v=exchg.150).aspx
data FolderProperty
    = FPFolderId
    | FPParentFolderId
    | FPDisplayName
    | FPUnreadCount
    | FPTotalCount
    | FPChildFolderCount
    | FPFolderClass
    | FPSearchParameters
    | FPManagedFolderInformation
    | FPPermissionSet
    | FPEffectiveRights
    | FPSharingEffectiveRights
    deriving (Eq, Show)

instance QC.Arbitrary FolderProperty where
    arbitrary = QC.elements
                [ FPFolderId
                , FPParentFolderId
                , FPDisplayName
                , FPUnreadCount
                , FPTotalCount
                , FPChildFolderCount
                , FPFolderClass
                , FPSearchParameters
                , FPManagedFolderInformation
                , FPPermissionSet
                , FPEffectiveRights
                , FPSharingEffectiveRights ]

-- | Renders a 'FolderProperty' as 'Text'.
--
-- https://msdn.microsoft.com/en-us/library/office/aa494315(v=exchg.150).aspx
--
-- prop> \fp -> Just fp == (folderPropertyFromText . folderPropertyToText) fp
folderPropertyToText :: FolderProperty -> Text
folderPropertyToText x = "folder:" <>
    case x of
        FPFolderId                 -> "FolderId"
        FPParentFolderId           -> "ParentFolderId"
        FPDisplayName              -> "DisplayName"
        FPUnreadCount              -> "UnreadCount"
        FPTotalCount               -> "TotalCount"
        FPChildFolderCount         -> "ChildFolderCount"
        FPFolderClass              -> "FolderClass"
        FPSearchParameters         -> "SearchParameters"
        FPManagedFolderInformation -> "ManagedFolderInformation"
        FPPermissionSet            -> "PermissionSet"
        FPEffectiveRights          -> "EffectiveRights"
        FPSharingEffectiveRights   -> "SharingEffectiveRights"

folderPropertyFromText :: Text -> Maybe FolderProperty
folderPropertyFromText txt =
    let
        (prefix, p) = T.splitAt 7 txt
    in
        if prefix /= "folder:"
        then Nothing
        else case p of
            "FolderId"                 -> Just FPFolderId
            "ParentFolderId"           -> Just FPParentFolderId
            "DisplayName"              -> Just FPDisplayName
            "UnreadCount"              -> Just FPUnreadCount
            "TotalCount"               -> Just FPTotalCount
            "ChildFolderCount"         -> Just FPChildFolderCount
            "FolderClass"              -> Just FPFolderClass
            "SearchParameters"         -> Just FPSearchParameters
            "ManagedFolderInformation" -> Just FPManagedFolderInformation
            "PermissionSet"            -> Just FPPermissionSet
            "EffectiveRights"          -> Just FPEffectiveRights
            "SharingEffectiveRights"   -> Just FPSharingEffectiveRights
            _                          -> Nothing

parseFolderProperty :: Text -> FEResult FolderProperty
parseFolderProperty text = case folderPropertyFromText text of
    Nothing -> FEFailure $ "Could not parse " <> text <> " as a FolderProperty"
    Just fp -> FESuccess fp

-------------------------------------------------------------------------------

-- | Additional properties for 'GetItem', 'UpdateItem', 'CreateItem',
-- 'FindItem' or 'FindFolder' requests.
--
-- https://msdn.microsoft.com/en-us/library/office/aa563810(v=exchg.150).aspx
--
-- TODO: Extend to include '[ExtendedFieldURI]' and '[IndexedFieldURI]' if
--       required.
--
-- >>> elemToText . toElement $ AdditionalProperties [FieldURIFolder FPFolderId]
-- "<AdditionalProperties><FieldURI FieldURI=\"folder:FolderId\"/></AdditionalProperties>"
--
-- prop> \a -> FESuccess a == (fromElement . toElement) (a :: AdditionalProperties)
data AdditionalProperties
    = AdditionalProperties [FieldURI]
    deriving (Eq, Show)

instance ToElement AdditionalProperties where
    toElement (AdditionalProperties us)
        = XML.Element "AdditionalProperties" Map.empty
        $ (XML.NodeElement . toElement) <$> us

instance FromElement AdditionalProperties where
    fromElement e@(XML.Element "AdditionalProperties" _ uriNodes) =
        AdditionalProperties <$> fieldURIs
      where
        fieldURIs = acceptOnlyChildrenWithName "FieldURI" e
                >>= sequence . fmap fromElement
    fromElement e = failElement "AdditionalProperties" e

instance QC.Arbitrary AdditionalProperties where
    arbitrary = AdditionalProperties <$> QC.arbitrary

-------------------------------------------------------------------------------

-- | Set of properties to return in an item or folder response.
--
-- https://msdn.microsoft.com/en-us/library/office/aa580545(v=exchg.150).aspx
--
-- >>> elemToText . toElement $ IdOnly
-- "<BaseShape>IdOnly</BaseShape>"
data BaseShape = IdOnly
               | Default
               | AllProperties
               deriving (Eq, Show)

instance ToElement BaseShape where
    toElement b = txtElement "BaseShape"
                $ case b of
                      IdOnly        -> "IdOnly"
                      Default       -> "Default"
                      AllProperties -> "AllProperties"

-------------------------------------------------------------------------------

-- | Folder properties to include in a 'GetFolder', 'FindFolder' or
-- 'SyncFolderHeirarchy' response.
--
-- https://msdn.microsoft.com/en-us/library/office/aa494311(v=exchg.150).aspx
--
-- >>> let s = FolderShape Default
-- >>> elemToText . toElement $ FolderShape Default Nothing
-- "<FolderShape><BaseShape>Default</BaseShape></FolderShape>"
data FolderShape
    = FolderShape BaseShape (Maybe AdditionalProperties)
    deriving (Eq, Show)

instance ToElement FolderShape where
    toElement (FolderShape bs ap)
        = XML.Element "FolderShape" Map.empty
        $ XML.NodeElement <$> catMaybes [Just $ toElement bs, toElement <$> ap]

-------------------------------------------------------------------------------

-- | Name of a mailbox user.
--
-- https://msdn.microsoft.com/en-us/library/office/aa564260(v=exchg.150).aspx
--
-- >>> elemToText . toElement $ Name "My User"
-- "<Name>My User</Name>"
data Name = Name Text deriving (Eq, Show)

instance IsString Name where
    fromString = Name . fromString

instance ToElement Name where
    toElement (Name n) = txtElement "Name" n

-- | Primary SMTP address of a mailbox user.
--
-- https://msdn.microsoft.com/en-us/library/office/aa565076(v=exchg.150).aspx
--
-- >>> elemToText . toElement $ EmailAddress "some@user.com"
-- "<EmailAddress>some@user.com</EmailAddress>"
data EmailAddress = EmailAddress Text deriving (Eq, Show)

instance IsString EmailAddress where
    fromString = EmailAddress . fromString

instance ToElement EmailAddress where
    toElement (EmailAddress e) = txtElement "EmailAddress" e
    
-- | Type of mailbox.
--
-- https://msdn.microsoft.com/en-us/library/office/aa563493(v=exchg.150).aspx
--
-- >>> elemToText . toElement $ MBPublicFolder
-- "<MailboxType>PublicFolder</MailboxType>"
data MailboxType
    = MBMailbox
    | MBPublicDL
    | MBPrivateDL
    | MBContact
    | MBPublicFolder
    | MBUnknown
    | MBOneOff
    | MBGroupMailbox
    deriving (Eq, Show)

instance ToElement MailboxType where
    toElement x = txtElement "MailboxType"
                $ case x of
                      MBMailbox      -> "Mailbox"
                      MBPublicDL     -> "PublicDL"
                      MBPrivateDL    -> "PrivateDL"
                      MBContact      -> "Contact"
                      MBPublicFolder -> "PublicFolder"
                      MBUnknown      -> "Unknown"
                      MBOneOff       -> "OneOff"
                      MBGroupMailbox -> "GroupMailbox"

-- | Identifies a mail-enabled Active Directory object.
--
-- https://msdn.microsoft.com/en-us/library/office/aa565036(v=exchg.150).aspx
--
-- >>> let b = Mailbox (Just "InBox") (Just "some@user.com") Nothing Nothing
-- >>> elemToText . toElement $ b
-- "<Mailbox><Name>InBox</Name><EmailAddress>some@user.com</EmailAddress></Mailbox>"
data Mailbox
    = Mailbox
        (Maybe Name)
        (Maybe EmailAddress)
        (Maybe MailboxType)
        (Maybe ItemId)
    deriving (Eq, Show)

instance ToElement Mailbox where
    toElement (Mailbox name address typ id)
        = XML.Element "Mailbox" Map.empty
        $ XML.NodeElement
      <$> catMaybes
        [ toElement <$> name
        , toElement <$> address
        , toElement <$> typ
        , toElement <$> id ]

-------------------------------------------------------------------------------

-- | Identifier and change key of a folder.
--
-- https://msdn.microsoft.com/en-us/library/office/aa579461(v=exchg.150).aspx
--
-- >>> let id = FolderId "ACDC42" (Just "FABB")
-- >>> elemToText . toElement $ id
-- "<FolderId ChangeKey=\"FABB\" Id=\"ACDC42\"/>"
data FolderId = FolderId Id (Maybe ChangeKey) deriving (Eq, Show)

instance ToElement FolderId where
    toElement (FolderId id mk) = XML.Element "FolderId" attrs []
      where
        attrs = Map.fromList
              $ catMaybes
              [ Just ("Id", idText id)
              , (\ck -> ("ChangeKey", changeKeyText ck)) <$> mk ]

-------------------------------------------------------------------------------

-- | Distinguished / well-known folders.
--
-- https://msdn.microsoft.com/en-us/library/office/aa580808(v=exchg.150).aspx
--
-- TODO: Complete this list as required.
--
-- >>> distinguishedFolderToText DFInbox
-- "inbox"
data DistinguishedFolder
    = DFInbox
    | DFRoot
    deriving (Eq, Show)

distinguishedFolderToText :: DistinguishedFolder -> Text
distinguishedFolderToText f =
    case f of
        DFInbox -> "inbox"
        DFRoot  -> "root"

-- | Identifies a distinguished folder that can be referenced by name.
--
-- https://msdn.microsoft.com/en-us/library/office/aa580808(v=exchg.150).aspx
--
-- >>> let mb = Mailbox Nothing Nothing Nothing Nothing
-- >>> let id = DistinguishedFolderId "ACDC42" Nothing mb
-- >>> elemToText . toElement $ id
-- "<DistinguishedFolderId Id=\"ACDC42\"><Mailbox/></DistinguishedFolderId>"
data DistinguishedFolderId
    = DistinguishedFolderId Id (Maybe ChangeKey) Mailbox
    deriving (Eq, Show)

instance ToElement DistinguishedFolderId where
    toElement (DistinguishedFolderId id mk b)
        = XML.Element "DistinguishedFolderId" attrs children
      where
        attrs = Map.fromList
              $ catMaybes
              [ Just ("Id", idText id)
              , (\ck -> ("ChangeKey", changeKeyText ck)) <$> mk ]
        children = XML.NodeElement <$> [toElement b]

-------------------------------------------------------------------------------

-- | List of folder identifiers used to identify folders to copy, move, get,
-- delete or monitor for event notifications.
--
-- https://msdn.microsoft.com/en-us/library/office/aa580509(v=exchg.150).aspx
--
-- >>> let mb = Mailbox Nothing Nothing Nothing Nothing
-- >>> let id = DistinguishedFolderId "ACDC" Nothing mb
-- >>> let fids = FolderIds [] [id]
-- >>> elemToText . toElement $ fids
-- "<FolderIds><DistinguishedFolderId Id=\"ACDC\"><Mailbox/></DistinguishedFolderId></FolderIds>"
data FolderIds = FolderIds [FolderId] [DistinguishedFolderId]
               deriving (Eq, Show)

instance ToElement FolderIds where
    toElement (FolderIds plain distinguished)
        = XML.Element "FolderIds" Map.empty
        $ XML.NodeElement
      <$> (toElement <$> plain) <> (toElement <$> distinguished)

-------------------------------------------------------------------------------

-- | Defines a request to get a folder from a mailbox in the Exchange store.
--
-- https://msdn.microsoft.com/en-us/library/office/aa580263(v=exchg.150).aspx
data GetFolder = GetFolder FolderShape FolderIds deriving (Show, Eq)

instance ToElement GetFolder where
    toElement (GetFolder shape ids)
        = XML.Element "GetFolder" Map.empty
        $ XML.NodeElement
      <$> [ toElement shape
          , toElement ids ]
