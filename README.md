# SendGrid-v3
[![Build Status](https://travis-ci.org/marcelbuesing/sendgrid-v3.svg?branch=dev)](https://travis-ci.org/marcelbuesing/sendgrid-v3)

A library for accessing the [v3 SendGrid API](https://sendgrid.com/docs/API_Reference/api_v3.html) in Haskell.

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.List.NonEmpty (fromList)
import Network.SendGridV3.Api
import Control.Lens ((^.))
import Network.Wreq (responseStatus, statusCode)

sendGridApiKey :: ApiKey
sendGridApiKey = ApiKey "SG..."

testMail :: Mail () ()
testMail =
  let to = personalization $ fromList [MailAddress "john@example.com" "John Doe"]
      from = MailAddress "jane@example.com" "Jane Smith"
      subject = "Email Subject"
      content = fromList [mailContentText "Example Content"]
  in mail [to] from subject content

main :: IO ()
main = do
  -- Send an email, overriding options as needed
  eResponse <- sendMail sendGridApiKey (testMail { _mailSendAt = Just 1516468000 })
  case eResponse of
    Left httpException -> error $ show httpException
    Right response -> print (response ^. responseStatus . statusCode)
```

# Test Setup
```
echo "export SENDGRID_API_KEY='SG.YOURKEY'" > sendgrid.env
echo "export SENDGRID_TEST_MAIL='target.email.address@doe.com' > sendgrid.env
source ./sendgrid.env
```
