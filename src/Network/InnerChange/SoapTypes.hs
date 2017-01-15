module Network.InnerChange.SoapTypes where

{-

import qualified Crypto.Hash     as Hash (Digest, SHA1, hash)
import qualified Data.ByteString.UTF8 as BS (fromString)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, singleton, fromList, lookup, (!), toList, keys)
import           Data.Maybe      (catMaybes, mapMaybe, isJust, fromJust)
import           Data.Monoid     ((<>))
import           Data.Proxy      (Proxy(Proxy))
import           Data.String     (IsString, fromString)
import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.Lazy  as LT (toStrict)
import qualified Data.Tuple      as Tuple (swap)
import           Data.Word       (Word32)
import qualified Test.QuickCheck as QC (Arbitrary, arbitrary, elements, listOf1,
                                       Gen)
import qualified Text.Read       as Read (readMaybe)
import qualified Text.XML        as XML (Document (Document), Element (Element),
                                         Name, Node (NodeContent, NodeElement,
                                                    NodeComment),
                                         Prologue (Prologue), def, renderText,
                                         elementName, elementAttributes,
                                         elementNodes, nameLocalName)

import Network.InnerChange.Soap.Types

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

nonEmptyText :: QC.Gen Text
nonEmptyText = T.pack <$> QC.listOf1 QC.arbitrary

word32ToText :: Word32 -> Text
word32ToText = T.pack . show . toInteger

textToWord32 :: Text -> Maybe Word32
textToWord32 = Read.readMaybe . T.unpack

parseWord32 :: Text -> FEResult Word32
parseWord32 t = case textToWord32 t of
    Just w  -> FESuccess w
    Nothing -> FEFailure $ "Could not parse " <> t <> " as a Word32."

invertMap :: (Ord v) => Map k v -> Map v k
invertMap = Map.fromList . (fmap Tuple.swap) . Map.toList

maybeToFEResult :: Text
                -> Maybe a
                -> FEResult a
maybeToFEResult failMsg ma = case ma of
    Just result -> FESuccess result
    Nothing     -> FEFailure failMsg

parseMaybeText :: Text               -- ^ name of thing we're trying to parse
               -> (Text -> Maybe a)  -- ^ simple Maybe-based parse function
               -> Text               -- ^ text we're trying to parse
               -> FEResult a         -- ^ result
parseMaybeText name f text = maybeToFEResult failMsg (f text)
  where
    failMsg = "Could not parse " <> text <> " as a " <> name <> "."

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

-- | Only processes elements which match the given name; otherwise fail.
matchElement :: XML.Name                      -- ^ name to match
             -> (XML.Element -> FEResult a)   -- ^ how to produce a result
             -> XML.Element                   -- ^ element to match on
             -> FEResult a                    -- ^ returned result
matchElement name f e = if eName == name
                        then f e
                        else failElement name e
  where
    eName = XML.elementName e

-- Only processes elements which match the given name and contain only text.
matchTxtElement :: XML.Name                 -- ^ name to match
                -> (Text -> FEResult a)     -- ^ how to produce a result
                -> XML.Element              -- ^ element to match on
                -> FEResult a               -- ^ returned result
matchTxtElement name f e = matchElement name f' e
  where
    f' el = acceptOnlyText el >>= f

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

-- | Returns a successful result containing the text from an element, only if
-- the element contains *just* text.
acceptOnlyText :: XML.Element -> FEResult Text
acceptOnlyText e = if allMatch then FESuccess text else failure
  where
    text = mconcat $ fromJust <$> childElements
    failure = FEFailure $ "Element "
                       <> elementName
                       <> "had non-text contents"
    elementName = T.pack . show $ XML.elementName e
    allMatch = all isJust childElements
    childElements = getContent <$> filter notComment (XML.elementNodes e)
    getContent n = case n of
        XML.NodeContent t -> Just t
        _                 -> Nothing
    notComment n = case n of
        XML.NodeComment _ -> False
        _                 -> True

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

-- | Returns all child elements with a given name, ignoring elements which
-- don't have that name.
getChildElementsWithName :: XML.Name -> XML.Element -> [XML.Element]
getChildElementsWithName name e = childElements
  where
    childElements = mapMaybe getElement (XML.elementNodes e)
    getElement n = case n of
        XML.NodeElement e@(XML.Element n _ _) ->
            if n == name
            then Just e
            else Nothing
        _ -> Nothing

-- | Returns the child element with a given name, which must be unique.
getUniqueChildElementWithName :: XML.Name -> XML.Element -> FEResult XML.Element
getUniqueChildElementWithName name e = case getChildElementsWithName name e of
    [x] -> FESuccess x
    _   -> FEFailure $ "Did not find exactly one element with name "
                    <> (T.pack . show) name
                    <> "."

-- | Returns an optional child element with a given name. If the optional child
-- element is present, it must be unique.
getOptionalUniqueChildElementWithName :: XML.Name
                                      -> XML.Element
                                      -> FEResult (Maybe XML.Element)
getOptionalUniqueChildElementWithName name e
    = case getChildElementsWithName name e of
          [x] -> FESuccess (Just x)
          []  -> FESuccess (Nothing)
          _   -> FEFailure $ "Did not find either zero or one instance of "
                          <> "an element with name "
                          <> (T.pack . show) name
                          <> "."

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

data Attr a = Attr (a -> ())

data IElement a
    = ITextElement a
      { itextName   :: XML.Name
      , itextEncode :: a -> Text
      , itextDecode :: Text -> Maybe a }
    | ITextSumElement a
      { itextSumName :: XML.Name
      , itextSumMap  :: Map a Text }
    | IAttrsElement a
      { iattrsName     :: XML.Name
      , iattrsRequired :: [Attr]
      , iattrsOptional :: [Attr] }

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
    fromElement = matchElement "ItemId" $
                  \e -> ItemId <$> getIdAttr e
                               <*> getChangeKeyOptionalAttr e

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
data FieldURI = FieldURIFolder FolderProperty deriving (Eq, Show)

instance ToElement FieldURI where
    toElement x = XML.Element "FieldURI" attr []
      where
        attr = Map.singleton "FieldURI"
             $ case x of
                   FieldURIFolder p -> folderPropertyToText p

instance FromElement FieldURI where
    fromElement = matchElement "FieldURI"
                $ \e -> FieldURIFolder <$> (   getAttr "FieldURI" e
                                           >>= parseFolderProperty )

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
    deriving (Eq, Show, Ord)

folderPropertyMap :: Map FolderProperty Text
folderPropertyMap
    = Map.fromList 
    $ fmap (\(k, v) -> (k, "folder:" <> v))
    [ (FPFolderId                 , "FolderId")
    , (FPParentFolderId           , "ParentFolderId")
    , (FPDisplayName              , "DisplayName")
    , (FPUnreadCount              , "UnreadCount")
    , (FPTotalCount               , "TotalCount")
    , (FPChildFolderCount         , "ChildFolderCount")
    , (FPFolderClass              , "FolderClass")
    , (FPSearchParameters         , "SearchParameters")
    , (FPManagedFolderInformation , "ManagedFolderInformation")
    , (FPPermissionSet            , "PermissionSet")
    , (FPEffectiveRights          , "EffectiveRights")
    , (FPSharingEffectiveRights   , "SharingEffectiveRights") ]

instance QC.Arbitrary FolderProperty where
    arbitrary = QC.elements $ Map.keys folderPropertyMap

-- | Renders a 'FolderProperty' as 'Text'.
--
-- https://msdn.microsoft.com/en-us/library/office/aa494315(v=exchg.150).aspx
--
-- prop> \fp -> Just fp == (folderPropertyFromText . folderPropertyToText) fp
folderPropertyToText :: FolderProperty -> Text
folderPropertyToText = (Map.!) folderPropertyMap

folderPropertyFromText :: Text -> Maybe FolderProperty
folderPropertyFromText = flip Map.lookup (invertMap folderPropertyMap)

parseFolderProperty :: Text -> FEResult FolderProperty
parseFolderProperty = parseMaybeText "FolderProperty" folderPropertyFromText

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
    fromElement = matchElement "AdditionalProperties"
                $ \e -> AdditionalProperties <$>
                        (   acceptOnlyChildrenWithName "FieldURI" e
                        >>= sequence . fmap fromElement )

instance QC.Arbitrary AdditionalProperties where
    arbitrary = AdditionalProperties <$> QC.listOf1 QC.arbitrary

-------------------------------------------------------------------------------

-- | Set of properties to return in an item or folder response.
--
-- https://msdn.microsoft.com/en-us/library/office/aa580545(v=exchg.150).aspx
--
-- >>> elemToText . toElement $ IdOnly
-- "<BaseShape>IdOnly</BaseShape>"
--
-- prop> \b -> FESuccess b == (fromElement . toElement) (b :: BaseShape)
data BaseShape = IdOnly
               | Default
               | AllProperties
               deriving (Eq, Show, Ord)

instance ElementTextSum BaseShape where
    elementTextSumName = const "BaseShape"
    elementTextMap = Map.fromList
        [ (IdOnly        , "IdOnly")
        , (Default       , "Default")
        , (AllProperties , "AllProperties") ]
{-
baseShapeToText :: BaseShape -> Text
baseShapeToText = (Map.!) baseShapeMap

textToBaseShape :: Text -> Maybe BaseShape
textToBaseShape = flip Map.lookup (invertMap baseShapeMap)

instance QC.Arbitrary BaseShape where
    arbitrary = QC.elements $ Map.keys baseShapeMap

instance ElementText BaseShape where
    elementTextName   = const "BaseShape"
    elementTextEncode = (Map.!) baseShapeMap
    elementTextDecode = flip Map.lookup (invertMap baseShapeMap)
-}

{-
instance ToElement BaseShape where
    toElement b = txtElement "BaseShape" (baseShapeToText b)

instance FromElement BaseShape where
    fromElement = matchTxtElement "BaseShape" parseBaseShape

instance QC.Arbitrary BaseShape where
    arbitrary = QC.elements [ IdOnly, Default, AllProperties ]
-}

-------------------------------------------------------------------------------

-- | Folder properties to include in a 'GetFolder', 'FindFolder' or
-- 'SyncFolderHeirarchy' response.
--
-- https://msdn.microsoft.com/en-us/library/office/aa494311(v=exchg.150).aspx
--
-- >>> let s = FolderShape Default
-- >>> elemToText . toElement $ FolderShape Default Nothing
-- "<FolderShape><BaseShape>Default</BaseShape></FolderShape>"
--
-- prop> \s -> FESuccess s == (fromElement . toElement) (s :: FolderShape)
data FolderShape
    = FolderShape BaseShape (Maybe AdditionalProperties)
    deriving (Eq, Show)

instance ToElement FolderShape where
    toElement (FolderShape bs ap)
        = XML.Element "FolderShape" Map.empty
        $ XML.NodeElement <$> catMaybes [Just $ toElement bs, toElement <$> ap]

instance FromElement FolderShape where
    fromElement = matchElement "FolderShape"
                $ \e -> FolderShape
                        <$> (   getUniqueChildElementWithName "BaseShape" e
                            >>= fromElement)
                        <*> (   getOptionalUniqueChildElementWithName
                                  "AdditionalProperties"
                                  e
                            >>= sequence . fmap fromElement)

instance QC.Arbitrary FolderShape where
    arbitrary = FolderShape <$> QC.arbitrary <*> QC.arbitrary

-------------------------------------------------------------------------------

-- | Name of a mailbox user.
--
-- https://msdn.microsoft.com/en-us/library/office/aa564260(v=exchg.150).aspx
--
-- >>> elemToText . toElement $ Name "My User"
-- "<Name>My User</Name>"
--
-- prop> \n -> FESuccess n == (fromElement . toElement) (n :: Name)
data Name = Name Text deriving (Eq, Show)

instance IsString Name where
    fromString = Name . fromString

instance ToElement Name where
    toElement (Name n) = txtElement "Name" n

instance FromElement Name where
    fromElement = matchTxtElement "Name" $ FESuccess . Name

instance QC.Arbitrary Name where
    arbitrary = Name <$> nonEmptyText

-- | Primary SMTP address of a mailbox user.
--
-- https://msdn.microsoft.com/en-us/library/office/aa565076(v=exchg.150).aspx
--
-- >>> elemToText . toElement $ EmailAddress "some@user.com"
-- "<EmailAddress>some@user.com</EmailAddress>"
--
-- prop> \e -> FESuccess e == (fromElement . toElement) (e :: EmailAddress)
data EmailAddress = EmailAddress Text deriving (Eq, Show)

instance IsString EmailAddress where
    fromString = EmailAddress . fromString

instance ToElement EmailAddress where
    toElement (EmailAddress e) = txtElement "EmailAddress" e

instance FromElement EmailAddress where
    fromElement = matchTxtElement "EmailAddress" $ FESuccess . EmailAddress

instance QC.Arbitrary EmailAddress where
    arbitrary = EmailAddress <$> nonEmptyText
    
-- | Type of mailbox.
--
-- https://msdn.microsoft.com/en-us/library/office/aa563493(v=exchg.150).aspx
--
-- >>> elemToText . toElement $ MBPublicFolder
-- "<MailboxType>PublicFolder</MailboxType>"
--
-- prop> \b -> FESuccess b == (fromElement . toElement) (b :: MailboxType)
data MailboxType
    = MBMailbox
    | MBPublicDL
    | MBPrivateDL
    | MBContact
    | MBPublicFolder
    | MBUnknown
    | MBOneOff
    | MBGroupMailbox
    deriving (Eq, Show, Ord)

mailboxMap :: Map MailboxType Text
mailboxMap = Map.fromList
    [ (MBMailbox      , "Mailbox")
    , (MBPublicDL     , "PublicDL")
    , (MBPrivateDL    , "PrivateDL")
    , (MBContact      , "Contact")
    , (MBPublicFolder , "PublicFolder")
    , (MBUnknown      , "Unknown")
    , (MBOneOff       , "OneOff")
    , (MBGroupMailbox , "GroupMailbox") ]

mailboxTypeToText :: MailboxType -> Text
mailboxTypeToText mbt = mailboxMap Map.! mbt

textToMailboxType :: Text -> Maybe MailboxType
textToMailboxType = flip Map.lookup (invertMap mailboxMap)

instance ToElement MailboxType where
    toElement x = txtElement "MailboxType" (mailboxTypeToText x)

instance FromElement MailboxType where
    fromElement = matchElement "MailboxType"
                $ \e -> acceptOnlyText e
                        >>= (parseMaybeText "MailboxType" textToMailboxType)

instance QC.Arbitrary MailboxType where
    arbitrary = QC.elements $ Map.keys mailboxMap

-- | Identifies a mail-enabled Active Directory object.
--
-- https://msdn.microsoft.com/en-us/library/office/aa565036(v=exchg.150).aspx
--
-- >>> let b = Mailbox (Just "InBox") (Just "some@user.com") Nothing Nothing
-- >>> elemToText . toElement $ b
-- "<Mailbox><Name>InBox</Name><EmailAddress>some@user.com</EmailAddress></Mailbox>"
--
-- prop> \m -> FESuccess m == (fromElement . toElement) (m :: Mailbox)
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

instance FromElement Mailbox where
    fromElement e@(XML.Element "Mailbox" _ _) = do
        ne <- getOptionalUniqueChildElementWithName "Name" e
        ee <- getOptionalUniqueChildElementWithName "EmailAddress" e
        me <- getOptionalUniqueChildElementWithName "MailboxType" e
        ie <- getOptionalUniqueChildElementWithName "ItemId" e
        n <- sequence $ fromElement <$> ne
        e <- sequence $ fromElement <$> ee
        m <- sequence $ fromElement <$> me
        i <- sequence $ fromElement <$> ie
        return $ Mailbox n e m i
    fromElement e = failElement "Mailbox" e

instance QC.Arbitrary Mailbox where
    arbitrary = Mailbox <$> QC.arbitrary
                        <*> QC.arbitrary
                        <*> QC.arbitrary
                        <*> QC.arbitrary

-------------------------------------------------------------------------------

-- | Identifier and change key of a folder.
--
-- https://msdn.microsoft.com/en-us/library/office/aa579461(v=exchg.150).aspx
--
-- >>> let id = FolderId "ACDC42" (Just "FABB")
-- >>> elemToText . toElement $ id
-- "<FolderId ChangeKey=\"FABB\" Id=\"ACDC42\"/>"
--
-- prop> \f -> FESuccess f == (fromElement . toElement) (f :: FolderId)
data FolderId = FolderId Id (Maybe ChangeKey) deriving (Eq, Show)

instance ToElement FolderId where
    toElement (FolderId id mk) = XML.Element "FolderId" attrs []
      where
        attrs = Map.fromList
              $ catMaybes
              [ Just ("Id", idText id)
              , (\ck -> ("ChangeKey", changeKeyText ck)) <$> mk ]

instance FromElement FolderId where
    fromElement e@(XML.Element "FolderId" _ _) = do
        id <- Id <$> getAttr "Id" e
        ck <- fmap ChangeKey <$> getOptionalAttr "ChangeKey" e
        return $ FolderId id ck
    fromElement e = failElement "FolderId" e

instance QC.Arbitrary FolderId where
    arbitrary = FolderId <$> QC.arbitrary <*> QC.arbitrary

-------------------------------------------------------------------------------

-- | Identifier and change key of a parent folder.
--
-- https://msdn.microsoft.com/en-us/library/office/aa494327(v=exchg.150).aspx
--
-- >>> let id = ParentFolderId "ACDC" (Just "FABB")
-- >>> elemToText . toElement $ id
-- "<ParentFolderId ChangeKey=\"FABB\" Id=\"ACDC\"/>"
--
-- prop> \p -> FESuccess p == (fromElement . toElement) (p :: ParentFolderId)
data ParentFolderId =
    ParentFolderId Id (Maybe ChangeKey) deriving (Eq, Show)

instance ToElement ParentFolderId where
    toElement (ParentFolderId id mk) = XML.Element "ParentFolderId" attrs []
      where
        attrs = Map.fromList
              $ catMaybes
              [ Just ("Id", idText id)
              , (\ck -> ("ChangeKey", changeKeyText ck)) <$> mk ]

instance FromElement ParentFolderId where
    fromElement e@(XML.Element "ParentFolderId" _ _)
        = ParentFolderId <$> (Id <$> getAttr "Id" e)
                         <*> (fmap ChangeKey <$> getOptionalAttr "ChangeKey" e)
    fromElement e = failElement "ParentFolderId" e

instance QC.Arbitrary ParentFolderId where
    arbitrary = ParentFolderId <$> QC.arbitrary <*> QC.arbitrary
    
-------------------------------------------------------------------------------

-- | Distinguished / well-known folders.
--
-- https://msdn.microsoft.com/en-us/library/office/aa580808(v=exchg.150).aspx
--
-- TODO: Complete this list as required.
--
-- >>> distinguishedFolderToText DFInbox
-- "inbox"
--
-- prop> \df -> FESuccess df == (parseDistinguishedFolder . distinguishedFolderToText) df
data DistinguishedFolder
    = DFInbox
    | DFRoot
    deriving (Eq, Show)

instance QC.Arbitrary DistinguishedFolder where
    arbitrary = QC.elements
                [ DFInbox
                , DFRoot ]

distinguishedFolderToText :: DistinguishedFolder -> Text
distinguishedFolderToText f =
    case f of
        DFInbox -> "inbox"
        DFRoot  -> "root"

distinguishedFolderFromText :: Text -> Maybe DistinguishedFolder
distinguishedFolderFromText txt = case txt of
    "inbox" -> Just DFInbox
    "root"  -> Just DFRoot
    _       -> Nothing

parseDistinguishedFolder :: Text -> FEResult DistinguishedFolder
parseDistinguishedFolder text = case distinguishedFolderFromText text of
    Just df -> FESuccess df
    Nothing -> FEFailure $ "Could not parse "
                        <> text
                        <> " as a DistinguishedFolder"

-- | Identifies a distinguished folder that can be referenced by name.
--
-- https://msdn.microsoft.com/en-us/library/office/aa580808(v=exchg.150).aspx
--
-- >>> let mb = Mailbox Nothing Nothing Nothing Nothing
-- >>> let id = DistinguishedFolderId DFInbox Nothing mb
-- >>> elemToText . toElement $ id
-- "<DistinguishedFolderId Id=\"inbox\"><Mailbox/></DistinguishedFolderId>"
--
-- prop> \d -> FESuccess d == (fromElement . toElement) (d :: DistinguishedFolderId)
data DistinguishedFolderId
    = DistinguishedFolderId DistinguishedFolder (Maybe ChangeKey) Mailbox
    deriving (Eq, Show)

instance ToElement DistinguishedFolderId where
    toElement (DistinguishedFolderId id mk b)
        = XML.Element "DistinguishedFolderId" attrs children
      where
        attrs = Map.fromList
              $ catMaybes
              [ Just ("Id", distinguishedFolderToText id)
              , (\ck -> ("ChangeKey", changeKeyText ck)) <$> mk ]
        children = XML.NodeElement <$> [toElement b]

instance FromElement DistinguishedFolderId where
    fromElement e@(XML.Element "DistinguishedFolderId" _ _) = do
        id <- getAttr "Id" e >>= parseDistinguishedFolder
        mk <- getChangeKeyOptionalAttr e
        mb <- getUniqueChildElementWithName "Mailbox" e >>= fromElement
        return $ DistinguishedFolderId id mk mb
    fromElement e = failElement "DistinguishedFolderId" e

instance QC.Arbitrary DistinguishedFolderId where
    arbitrary = DistinguishedFolderId
            <$> QC.arbitrary
            <*> QC.arbitrary
            <*> QC.arbitrary

-------------------------------------------------------------------------------

-- | List of folder identifiers used to identify folders to copy, move, get,
-- delete or monitor for event notifications.
--
-- https://msdn.microsoft.com/en-us/library/office/aa580509(v=exchg.150).aspx
--
-- >>> let mb = Mailbox Nothing Nothing Nothing Nothing
-- >>> let id = DistinguishedFolderId DFInbox Nothing mb
-- >>> let fids = FolderIds [] [id]
-- >>> elemToText . toElement $ fids
-- "<FolderIds><DistinguishedFolderId Id=\"inbox\"><Mailbox/></DistinguishedFolderId></FolderIds>"
--
-- prop> \i -> FESuccess i == (fromElement . toElement) (i :: FolderIds)
data FolderIds = FolderIds [FolderId] [DistinguishedFolderId]
               deriving (Eq, Show)

instance ToElement FolderIds where
    toElement (FolderIds plain distinguished)
        = XML.Element "FolderIds" Map.empty
        $ XML.NodeElement
      <$> (toElement <$> plain) <> (toElement <$> distinguished)

instance FromElement FolderIds where
    fromElement e@(XML.Element "FolderIds" _ _) = do
        let
            plain         = getChildElementsWithName "FolderId" e
            distinguished = getChildElementsWithName "DistinguishedFolderId" e
        ps <- sequence $ fromElement <$> plain
        ds <- sequence $ fromElement <$> distinguished    
        return $ FolderIds ps ds
    fromElement e = failElement "FolderIds" e

instance QC.Arbitrary FolderIds where
    arbitrary = FolderIds <$> QC.arbitrary <*> QC.arbitrary

-------------------------------------------------------------------------------

-- | Defines a request to get a folder from a mailbox in the Exchange store.
--
-- https://msdn.microsoft.com/en-us/library/office/aa580263(v=exchg.150).aspx
--
-- prop> \f -> FESuccess f == (fromElement . toElement) (f :: GetFolder)
data GetFolder = GetFolder FolderShape FolderIds deriving (Show, Eq)

instance ToElement GetFolder where
    toElement (GetFolder shape ids)
        = XML.Element "GetFolder" Map.empty
        $ XML.NodeElement
      <$> [ toElement shape
          , toElement ids ]

instance FromElement GetFolder where
    fromElement e@(XML.Element "GetFolder" _ _) =
        GetFolder <$> (   getUniqueChildElementWithName "FolderShape" e
                      >>= fromElement )
                  <*> (   getUniqueChildElementWithName "FolderIds" e
                      >>= fromElement )
    fromElement e = failElement "GetFolder" e

instance QC.Arbitrary GetFolder where
    arbitrary = GetFolder <$> QC.arbitrary <*> QC.arbitrary

-------------------------------------------------------------------------------

-- | Text description and status of a response.
--
-- The 'MessageText' is not required and not included in all responses. It is
-- usually included when error messages are returned.
--
-- https://msdn.microsoft.com/en-us/library/office/aa581040(v=exchg.150).aspx
--
-- >>> elemToText . toElement $ MessageText "Hello World"
-- "<MessageText>Hello World</MessageText>"
--
-- prop> \m -> FESuccess m == (fromElement . toElement) (m :: MessageText)
data MessageText = MessageText Text deriving (Eq, Show)

instance ToElement MessageText where
    toElement (MessageText text) = txtElement "MessageText" text

instance FromElement MessageText where
    fromElement e@(XML.Element "MessageText" _ _) =
        MessageText <$> acceptOnlyText e
    fromElement e = failElement "MessageText" e

instance QC.Arbitrary MessageText where
    arbitrary = MessageText <$> nonEmptyText

-------------------------------------------------------------------------------

-- | Response types.
--
-- TODO: Complete this data type as required.
-- 
-- https://msdn.microsoft.com/en-us/library/office/aa580757(v=exchg.150).aspx
--
-- prop> \r -> FESuccess r == (parseResponse . responseToText) r
data Response
    = NoError
    | ErrorAccessDenied
    | ErrorFolderNotFound
    deriving (Eq, Show)

instance QC.Arbitrary Response where
    arbitrary = QC.elements
        [ NoError
        , ErrorAccessDenied
        , ErrorFolderNotFound ]

responseToText :: Response -> Text
responseToText r = case r of
    NoError             -> "NoError"
    ErrorAccessDenied   -> "ErrorAccessDenied"
    ErrorFolderNotFound -> "ErrorFolderNotFound"

textToResponse :: Text -> Maybe Response
textToResponse t = case t of
    "NoError"             -> Just NoError
    "ErrorAccessDenied"   -> Just ErrorAccessDenied
    "ErrorFolderNotFound" -> Just ErrorFolderNotFound
    _                     -> Nothing

parseResponse :: Text -> FEResult Response
parseResponse t = case textToResponse t of
    Just r -> FESuccess r
    Nothing -> FEFailure $ "Could not parse "
                        <> t
                        <> " as a Response."

-- | Status information about a request.
--
-- https://msdn.microsoft.com/en-us/library/office/aa580757(v=exchg.150).aspx
--
-- >>> elemToText . toElement $ ResponseCode NoError
-- "<ResponseCode>NoError</ResponseCode>"
--
-- prop> \r -> FESuccess r == (fromElement . toElement) (r :: ResponseCode)
data ResponseCode = ResponseCode Response deriving (Eq, Show)

instance ToElement ResponseCode where
    toElement (ResponseCode r) = txtElement "ResponseCode" (responseToText r)

instance FromElement ResponseCode where
    fromElement e@(XML.Element "ResponseCode" _ _) =
        ResponseCode <$> (acceptOnlyText e >>= parseResponse)
    fromElement e = failElement "ResponseCode" e

instance QC.Arbitrary ResponseCode where
    arbitrary = ResponseCode <$> QC.arbitrary

-------------------------------------------------------------------------------

-- | The IPF folder class of a folder.
--
-- https://msdn.microsoft.com/en-us/library/office/dn535505(v=exchg.150).aspx
--
-- prop> \i -> FESuccess i == (parseIPF . ipfToText) i
data IPF
    = IPFNote         -- ^ Email messages or folders.
    | IPFAppointment
    | IPFContact
    | IPFTask
    deriving (Eq, Show)

instance QC.Arbitrary IPF where
    arbitrary = QC.elements
                [ IPFNote
                , IPFAppointment
                , IPFContact
                , IPFTask ]

ipfToText :: IPF -> Text
ipfToText i = case i of
    IPFNote        -> "IPF.Note"
    IPFAppointment -> "IPF.Appointment"
    IPFContact     -> "IPF.Contact"
    IPFTask        -> "IPF.Task"

textToIPF :: Text -> Maybe IPF
textToIPF t = case t of
    "IPF.Note"        -> Just IPFNote
    "IPF.Appointment" -> Just IPFAppointment
    "IPF.Contact"     -> Just IPFContact
    "IPF.Task"        -> Just IPFTask
    _                 -> Nothing

parseIPF :: Text -> FEResult IPF
parseIPF t = case textToIPF t of
    Just ipf -> FESuccess ipf
    Nothing -> FEFailure $ "Could not parse " <> t <> " as an IPF."

-- | The folder class of a folder.
--
-- https://msdn.microsoft.com/en-us/library/office/aa579453(v=exchg.150).aspx
--
-- >>> elemToText . toElement $ FolderClass IPFNote
-- "<FolderClass>IPF.Note</FolderClass>"
--
-- prop> \f -> FESuccess f == (fromElement . toElement) (f :: FolderClass)
data FolderClass = FolderClass IPF deriving (Eq, Show)

instance ToElement FolderClass where
    toElement (FolderClass ipf) = txtElement "FolderClass" (ipfToText ipf)

instance FromElement FolderClass where
    fromElement e@(XML.Element "FolderClass" _ _) =
        FolderClass <$> (acceptOnlyText e >>= parseIPF)
    fromElement e = failElement "FolderClass" e

instance QC.Arbitrary FolderClass where
    arbitrary = FolderClass <$> QC.arbitrary

-------------------------------------------------------------------------------

-- | Display name of a folder, contact, distribution list, delegate user,
-- location or rule.
--
-- https://msdn.microsoft.com/en-us/library/office/aa566011(v=exchg.150).aspx
--
-- >>> elemToText . toElement $ DisplayName "Inbox"
-- "<DisplayName>Inbox</DisplayName>"
--
-- prop> \d -> FESuccess d == (fromElement . toElement) (d :: DisplayName)
data DisplayName = DisplayName Text deriving (Eq, Show)

instance ToElement DisplayName where
    toElement (DisplayName n) = txtElement "DisplayName" n

instance FromElement DisplayName where
    fromElement e@(XML.Element "DisplayName" _ _)
        = DisplayName <$> acceptOnlyText e
    fromElement e = failElement "DisplayName" e

instance QC.Arbitrary DisplayName where
    arbitrary = DisplayName <$> nonEmptyText

-------------------------------------------------------------------------------

-- | Total count of items within a given folder.
--
-- https://msdn.microsoft.com/en-us/library/office/aa565243(v=exchg.150).aspx
--
-- >>> elemToText . toElement $ TotalCount 10
-- "<TotalCount>10</TotalCount>"
--
-- prop> \c -> FESuccess c == (fromElement . toElement) (c :: TotalCount)
data TotalCount = TotalCount Word32 deriving (Eq, Show)

instance ToElement TotalCount where
    toElement (TotalCount n) = txtElement "TotalCount" (word32ToText n)

instance FromElement TotalCount where
    fromElement e@(XML.Element "TotalCount" _ _)
        = TotalCount <$> (acceptOnlyText e >>= parseWord32)
    fromElement e = failElement "TotalCount" e

instance QC.Arbitrary TotalCount where
    arbitrary = TotalCount <$> QC.arbitrary

-------------------------------------------------------------------------------

-- | Number of immediate child folders contained within a folder.
--
-- https://msdn.microsoft.com/en-us/library/office/aa565882(v=exchg.150).aspx
--
-- >>> elemToText . toElement $ ChildFolderCount 42
-- "<ChildFolderCount>42</ChildFolderCount>"
--
-- prop> \c -> FESuccess c == (fromElement . toElement) (c :: ChildFolderCount)
data ChildFolderCount = ChildFolderCount Word32 deriving (Eq, Show)

instance ToElement ChildFolderCount where
    toElement (ChildFolderCount n)
        = txtElement "ChildFolderCount" (word32ToText n)

instance FromElement ChildFolderCount where
    fromElement e@(XML.Element "ChildFolderCount" _ _)
        = ChildFolderCount <$> (acceptOnlyText e >>= parseWord32)
    fromElement e = failElement "ChildFolderCount" e

instance QC.Arbitrary ChildFolderCount where
    arbitrary = ChildFolderCount <$> QC.arbitrary

-}
