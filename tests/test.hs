import Data.Maybe (fromJust)
import System.Environment
import Test.Tasty
import Test.Tasty.HUnit
import Network.SendGridV3.Api
import Data.Text as T

main = do
  sendgridKey  <- getSendGridKey
  testMailAddr <- getTestEmailAddress
  defaultMain $
    testCase "Send email cc" $ do
    -- assertion no. 1 (passes)
    1 @?= 2
    --sendMail

getSendGridKey :: IO ApiKey
getSendGridKey = do
  envKey <- lookupEnv "SENDGRID_APIKEY"
  case envKey of
    Nothing -> error
      "Please supply a Sendgrid api key for testing via the ENV var `SENDGRID_APIKEY`"
    Just k -> return $ ApiKey $ T.pack k

getTestEmailAddress :: IO MailAddress
getTestEmailAddress = do
  envKey <- lookupEnv "SENDGRID_TEST_MAIL"
  case envKey of
    Nothing -> error
      "Please supply an email address for testing via the ENV var `SENDGRID_TEST_MAIL`"
    Just k -> return $ MailAddress $ T.pack k
