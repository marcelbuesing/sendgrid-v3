module Network.SendGridV3.JSON where

import           Data.Aeson                               ( camelTo2 )

-- | Format a prefixed record field for SendGrid API consumption
unPrefix :: String -> String -> String
unPrefix prefix = camelTo2 '_' . drop (length prefix)
