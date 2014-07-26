--Connor Cormier, 7/26/14
--Replacement for older gmail module that connects to a local smtp server.

{-# LANGUAGE OverloadedStrings #-}

module LocalSmtpClient where

import Network.Mail.SMTP
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Control.Concurrent

fromAddr = Address (Just "Snys Server") "snys@raspbi.mooo.com"

sendEmail :: String -> String -> String -> IO ()
sendEmail to subject body =
   do sendMail
         "localhost"
         (simpleMail fromAddr 
            [Address Nothing $ T.pack to]
            []
            []
            (T.pack subject)
            [plainTextPart $ L.pack body])

fSendEmail :: String -> String -> String -> IO ThreadId
fSendEmail to subject body = forkIO $ sendEmail to subject body
