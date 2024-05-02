{-# LANGUAGE OverloadedStrings #-}

import Data.List.NonEmpty (fromList)
import Data.Text (Text)
import Network.SendGridV3.Api
import Control.Lens ((^.))
import Network.Wreq (responseStatus, statusCode)

sendGridApiKey :: Text
sendGridApiKey = "SG..."

testMail :: Mail () ()
testMail =
  let to = personalization $ fromList [MailAddress "john@example.com" "John Doe"]
      from = MailAddress "jane@example.com" "Jane Smith"
      subject = "Email Subject"
      content = fromList [mailContentText "Example Content"]
  in mail [to] from subject $ Just content

main :: IO ()
main = do
  sendgridSettings <- mkSendGridSettings sendGridApiKey

  -- Send an email, overriding options as needed
  eResponse <- sendMail sendgridSettings (testMail { _mailSendAt = Just 1516468000 })
  case eResponse of
    Left httpException -> error $ show httpException
    Right response -> print (response ^. responseStatus . statusCode)
