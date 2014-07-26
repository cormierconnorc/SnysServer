--Connor Cormier, 7/13/14

--Note: you'd think I could cut out those packs with OverloadedStrings, but it doesn't work...

module EmailClient where

import Network.Mail.Mime
import Network.Mail.Client.Gmail
import qualified Data.Text.Lazy as L
import qualified Data.Text as S
import Control.Applicative
import Control.Concurrent

credentialsFile = "credentials.txt"

readCredentials :: IO (String, String)
readCredentials = 
   do content <- readFile credentialsFile
      let (email:pass:_) = filter validLine . lines $ content
      return (email, pass)
   where validLine = (&&) <$> ((> 0) . length) <*> ((/= '#') . head)

sendEmail :: String -> String -> String -> IO ()
sendEmail to subject body =
   do (gmailUser, gmailPass) <- readCredentials
      sendGmail
        (L.pack gmailUser)
        (L.pack gmailPass)
        (Address (Just $ S.pack "Snys") $ S.pack gmailUser)
        [Address Nothing (S.pack to)] [] []
        (S.pack subject)
        (L.pack body)
        []

--Send mail in a new thread
fSendEmail :: String -> String -> String -> IO ThreadId
fSendEmail to subject body = forkIO $ sendMail to subject body
