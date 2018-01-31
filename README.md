# SendGrid-v3
[![Build Status](https://travis-ci.org/marcelbuesing/sendgrid-v3.svg?branch=dev)](https://travis-ci.org/marcelbuesing/sendgrid-v3)

A library for accessing the [v3 SendGrid API](https://sendgrid.com/docs/API_Reference/api_v3.html) in Haskell.

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.List.NonEmpty (fromList)
import Network.SendGridV3.Api

sendGridApiKey :: ApiKey
sendGridApiKey = "SG..."

testMail :: Mail () ()
testMail addr =
  let from'    = "john.doe+from@doe.com"
      subject' = "Email Subject"
      content' = fromList [mailContentText "Example Content"]
  in mail [] from' subject' content'

main :: IO ()
main = do
  -- Simple Send
  statusCode <- sendMail sendGridApiKey testMail
  -- Send with further options
  statusCode <- sendMail sendGridApiKey (testMail { _mailSendAt = Just 1516468000 })
  return ()
```

# Test Setup
```
echo "export SENDGRID_API_KEY='SG.YOURKEY'" > sendgrid.env
echo "export SENDGRID_TEST_MAIL='target.email.address@doe.com' > sendgrid.env
source ./sendgrid.env
```
