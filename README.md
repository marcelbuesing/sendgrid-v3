# SendGrid-v3
A library for accessing the [v3 SendGrid API](https://sendgrid.com/docs/API_Reference/api_v3.html) in Haskell.

```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.List.NonEmpty (fromList)
import Network.SendGridV3.Api

testMail :: Mail () ()
testMail addr =
  let from'    = "john.doe+from@doe.com"
      subject' = "Email Subject"
      content' = fromList [mailContentText "Example Content"]
  in mail [] from' subject' content'

main :: IO ()
main = do
  statusCode <- sendMail sendgridKey (testMail testMailAddr)
  return ()
```

# Test Setup
```
echo "export SENDGRID_API_KEY='SG.YOURKEY'" > sendgrid.env
echo "export SENDGRID_TEST_MAIL='target.email.address@doe.com' > sendgrid.env
source ./sendgrid.env
```

