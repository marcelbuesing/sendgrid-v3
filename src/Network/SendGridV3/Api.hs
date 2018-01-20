{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | https://sendgrid.com/docs/API_Reference/api_v3.html
module Network.SendGridV3.Api where

import Control.Lens hiding ((.=))
import Control.Lens.TH
import Data.Aeson
import Data.List.NonEmpty
import Data.Semigroup ((<>))
import Data.Text as T
import Data.Text.Encoding
import Network.Wreq

sendGridAPI :: T.Text
sendGridAPI = "https://api.sendgrid.com/v3/mail/send"

-- | Bearer Token for the API
data ApiKey = ApiKey { _apiKey :: T.Text } deriving (Show, Eq);

data MailAddress = MailAddress
  { -- | EmailAddress e.g. john@doe.com
    _mailAddressEmail :: T.Text
    -- | The name of the person to whom you are sending an email. E.g. "John Doe"
  , _mailAddressName  :: T.Text
  } deriving (Show, Eq)

instance ToJSON MailAddress where
  toJSON (MailAddress e n) =
    object [ "email" .= e
           , "name"  .= n
           ]

data MailContent = MailContent
  { -- | The mime type of the content you are including in your email. For example, “text/plain” or “text/html”.
    _mailContentType  :: T.Text
    -- | The actual content of the specified mime type that you are including in your email.
  , _mailContentValue :: T.Text
  } deriving (Show, Eq)

instance ToJSON MailContent where
  toJSON (MailContent typ val) =
    object [ "type"  .= typ
           , "value" .= val
           ]

-- | An array of messages and their metadata. Each object within personalizations can be thought of as an envelope
-- | - it defines who should receive an individual message and how that message should be handled.
data Personalization = Personalization
  { _personalizationTo      :: [MailAddress]
  , _personalizationCc      :: [MailAddress]
  , _personalizationBcc     :: [MailAddress]
  , _personalizationSubject :: T.Text
  } deriving (Show, Eq)

instance ToJSON Personalization where
  toJSON (Personalization to cc bcc sub) =
    object [ "to"      .= to
           , "cc"      .= cc
           , "bcc"     .= bcc
           , "subject" .= sub
           ]

-- | The content-disposition of the attachment specifying how you would like the attachment to be displayed.
data Disposition =
  -- | Results in the attached file being displayed automatically within the message.
    Inline
  -- | Results in the attached file requiring some action to be taken before it is
  -- | displayed (e.g. opening or downloading the file).
  | Attachment
  deriving (Show, Eq)

data MailAttachment = MailAttachment
  { -- | The Base64 encoded content of the attachment.
    _mailAttachmentContent     :: T.Text
    -- | The mime type of the content you are attaching. For example, “text/plain” or “text/html”.
  , _mailAttachmentType        :: Maybe T.Text
    -- | The filename of the attachment.
  , _mailAttachmentFilename    :: T.Text
  -- | The content-disposition of the attachment specifying how you would like the attachment to be displayed.
  , _mailAttachmentDisposition :: Maybe Disposition
  -- | The content id for the attachment. This is used when the disposition is set to “inline”
  -- | and the attachment is an image, allowing the file to be displayed within the body of your email.
  , _mailAttachmentContentId   :: T.Text
  } deriving (Show, Eq)

-- | An object allowing you to specify how to handle unsubscribes.
data Asm = Asm
  { -- | The unsubscribe group to associate with this email.
    _asmGroupId :: Int
    -- | An array containing the unsubscribe groups that you would like to
    -- | be displayed on the unsubscribe preferences page.
  , _asmGroupsToDisplay :: Maybe [Int]
  } deriving (Show, Eq)

data Mail a b = Mail
  { -- | An array of messages and their metadata.
    -- | Each object within personalizations can be thought of as an envelope
    -- | - it defines who should receive an individual message and how that message should be handled.
    _mailPersonalizations :: [Personalization]
  , _mailFrom             :: MailAddress
  , _mailReplyTo          :: MailAddress
  , _mailSubject          :: T.Text
  -- | An array in which you may specify the content of your email.
  -- | You can include multiple mime types of content, but you must specify at least one mime type.
  , _mailContent          :: NonEmpty MailContent
  -- | An array of objects in which you can specify any attachments you want to include.
  , _mailAttachments      :: Maybe [MailAttachment]
  -- | The id of a template that you would like to use.
  -- | If you use a template that contains a subject and content (either text or html),
  -- | you do not need to specify those at the personalizations nor message level.
  , _mailTemplateId       :: Maybe T.Text
  -- | An object of key/value pairs that define block sections of code to be used as substitutions.
  , _mailSections         :: Maybe a
  -- | An object containing key/value pairs of header names and the value to substitute for them.
  -- | You must ensure these are properly encoded if they contain unicode characters.
  -- | Must not be one of the reserved headers.
  , _mailHeaders          :: Maybe [(T.Text, T.Text)]
  -- | An array of category names for this message. Each category name may not exceed 255 characters.
  , _mailCategories       :: Maybe [T.Text]
  -- | Values that are specific to the entire send that will be carried along with
  -- | the email and its activity data.
  , _mailCustomArgs       :: Maybe b
  -- | A unix timestamp allowing you to specify when you want your email to be delivered.
  , _mailSendAt           :: Maybe Int
  -- | This ID represents a batch of emails to be sent at the same time. Including a batch_id
  -- | in your request allows you include this email in that batch, and also enables you to
  -- | cancel or pause the delivery of that batch. For more information,
  -- | see https://sendgrid.com/docs/API_Reference/Web_API_v3/cancel_schedule_send.html
  , _mailBatchId          :: Maybe T.Text
  -- | An object allowing you to specify how to handle unsubscribes.
  , _mailAsm              :: Maybe Asm
  -- | The IP Pool that you would like to send this email from.
  , _mailIpPoolName       :: Maybe T.Text
--  -- | A collection of different mail settings that you can use to specify how you would
--  -- | like this email to be handled.
--  , _mailMailSettings     :: MailSettings
--  -- | Settings to determine how you would like to track the metrics of how your recipients
--  -- | interact with your email.
--  , _mailTrackingSettings :: MailTrackingSettings
  } deriving (Show, Eq)

makeLenses ''Mail

instance ToJSON (Mail a b) where
  toJSON m =
    object [ "personalizations" .= (m ^. mailPersonalizations)
           , "from"             .= (m ^. mailFrom)
           , "reply_to"         .= (m ^. mailReplyTo)
           , "subject"          .= (m ^. mailSubject)
           , "content"          .= (m ^. mailContent)
           ]

-- | Send an email via the SendGrid API.
-- | `a` -  Type of Mail Section, see `_mailSections` for details
-- | `b` -  Type of Custom Arg, see `_mailCustomArgs` for details
sendMail :: (ToJSON a, ToJSON b) => ApiKey -> Mail a b -> IO Status
sendMail (ApiKey key) mail = do
  let tkn = encodeUtf8 $ "Bearer " <> key
      opts = defaults &
          (header "Authorization" .~ [tkn])
        . (header "Content-Type" .~ ["application/json"])
  r <- postWith opts (T.unpack sendGridAPI) (toJSON mail)
  return $ r ^. responseStatus
