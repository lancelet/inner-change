{-# LANGUAGE OverloadedStrings #-}

module Network.InnerChange.SoapTypes where

import qualified Data.Map.Strict as Map (empty, singleton)
import           Data.Maybe      (catMaybes)
import           Data.Monoid     ((<>))
import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.Lazy  as LT (toStrict)
import qualified Text.XML        as XML (Document (Document), Element (Element),
                                         Name, Node (NodeContent, NodeElement),
                                         Prologue (Prologue), def, renderText)


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

-- | Serialization of some type 'a' to an 'XML.Element'.
class ToElement a where
    toElement :: a -> XML.Element

-------------------------------------------------------------------------------

-- | FieldURI
--
-- https://msdn.microsoft.com/en-us/library/office/aa494315(v=exchg.150).aspx
--
-- TODO: extend with properties for item, message, etc.
--
-- >>> elemToText . toElement $ FieldURIFolder FPFolderId
-- "<FieldURI FieldURI=\"folder:FolderId\"/>"
data FieldURI
    = FieldURIFolder FolderProperty

instance ToElement FieldURI where
    toElement x = XML.Element "FieldURI" attr []
      where
        attr = Map.singleton "FieldURI"
             $ case x of
                   FieldURIFolder p -> folderPropertyToText p

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

-- | Renders a 'FolderProperty' as 'Text'.
--
-- https://msdn.microsoft.com/en-us/library/office/aa494315(v=exchg.150).aspx
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
data AdditionalProperties
    = AdditionalProperties [FieldURI]

instance ToElement AdditionalProperties where
    toElement (AdditionalProperties us)
        = XML.Element "AdditionalProperties" Map.empty
        $ (XML.NodeElement . toElement) <$> us

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

instance ToElement Name where
    toElement (Name n) = txtElement "name" n



{-
data DistinguishedFolder
    = DFInbox
    | DFRoot
    deriving (Eq, Show)

distinguishedFolderToText 
-}
