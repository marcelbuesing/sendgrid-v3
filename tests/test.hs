{-# LANGUAGE OverloadedStrings #-}

import           Control.Lens                             ( (^.) )
import           Data.List.NonEmpty                       ( fromList )
import           Data.Text                     as T
import           Network.SendGridV3.Api
import           Network.Wreq
import           System.Environment
import           Test.Tasty
import           Test.Tasty.HUnit

testMail :: MailAddress -> Mail () ()
testMail addr = mail [personalization (fromList [addr])]
                     addr
                     "Mail Subject"
                     (Just $ fromList [mailContentText "Test Content"])

main :: IO ()
main = do
  sendgridKey  <- getSendGridKey
  testMailAddr <- getTestEmailAddress
  defaultMain $ testGroup
    "SendGrid v3 API"
    [ testCase "Send email simple" $ do
      eResponse <- sendMail sendgridKey (testMail testMailAddr)
      case eResponse of
        Left  err -> error "Failed to send simple email"
        Right r   -> r ^. responseStatus . statusCode @?= 202
    , testCase "Send email with opts" $ do
      eResponse <- sendMail
        sendgridKey
        ((testMail testMailAddr) { _mailSendAt = Just 1516468000 })
      case eResponse of
        Left  err -> error "Failed to send email with opts"
        Right r   -> r ^. responseStatus . statusCode @?= 202
    , testCase "Send an email payload with categories correctly" $ do
      let email =
            (testMail testMailAddr) { _mailCategories = Just ["fake-category"] }
      eResponse <- sendMail sendgridKey email
      case eResponse of
        Left  err -> error "Failed to send email with opts"
        Right r   -> r ^. responseStatus . statusCode @?= 202
    ]

getSendGridKey :: IO ApiKey
getSendGridKey = do
  envKey <- lookupEnv "SENDGRID_API_KEY"
  case envKey of
    Nothing ->
      error
        "Please supply a Sendgrid api key for testing via the ENV var `SENDGRID_API_KEY`"
    Just k -> return $ ApiKey $ T.pack k

getTestEmailAddress :: IO MailAddress
getTestEmailAddress = do
  envAddr <- lookupEnv "SENDGRID_TEST_MAIL"
  case envAddr of
    Nothing ->
      error
        "Please supply an email address for testing via the ENV var `SENDGRID_TEST_MAIL`"
    Just a -> return $ MailAddress (T.pack a) "John Doe"
