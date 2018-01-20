{-# LANGUAGE OverloadedStrings #-}

import Data.List.NonEmpty (fromList)
import Data.Text as T
import Network.SendGridV3.Api
import System.Environment
import Test.Tasty
import Test.Tasty.HUnit

testMail :: MailAddress -> Mail () ()
testMail addr =
  mail [personalization (fromList [addr])] addr "Mail Subject" (fromList [mailContentText "Test Content"])

main :: IO ()
main = do
  sendgridKey  <- getSendGridKey
  testMailAddr <- getTestEmailAddress
  defaultMain $
    testCase "Send email simple" $ do
    statusCode <- sendMail sendgridKey (testMail testMailAddr)
    statusCode @?= 202

getSendGridKey :: IO ApiKey
getSendGridKey = do
  envKey <- lookupEnv "SENDGRID_API_KEY"
  case envKey of
    Nothing -> error
      "Please supply a Sendgrid api key for testing via the ENV var `SENDGRID_API_KEY`"
    Just k -> return $ ApiKey $ T.pack k

getTestEmailAddress :: IO MailAddress
getTestEmailAddress = do
  envAddr <- lookupEnv "SENDGRID_TEST_MAIL"
  case envAddr of
    Nothing -> error
      "Please supply an email address for testing via the ENV var `SENDGRID_TEST_MAIL`"
    Just a -> return $ MailAddress (T.pack a) "John Doe"


