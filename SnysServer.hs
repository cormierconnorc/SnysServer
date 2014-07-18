--Connor Cormier, 7/14/14

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Happstack.Server
import qualified DatabaseClient as Db
import EmailClient
import Control.Concurrent
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad
import Control.Monad.IO.Class
import Database.HDBC.ODBC
import Data.List (isInfixOf, groupBy)
import Data.Aeson.Encode
import Data.Aeson.Types
import Data.Aeson.TH
import qualified Data.Vector as V
import Data.Maybe (isJust, isNothing)
import Control.Applicative (optional)
import Data.Time.Clock.POSIX
import Data.Function (on)

data GenericResponse = GenericResponse { error :: String,
                                         response :: String
                                       } deriving (Show, Eq)

--Create a default ToJSON instance for GenericResponse
$(deriveToJSON defaultOptions ''GenericResponse) 
--JSON instances for Database types
$(deriveToJSON defaultOptions ''Db.UserStatus)
$(deriveToJSON defaultOptions ''Db.MembershipPermission)
$(deriveToJSON defaultOptions ''Db.UserNoteStatus)
$(deriveToJSON defaultOptions ''Db.Group)
$(deriveToJSON defaultOptions ''Db.Membership)
$(deriveToJSON defaultOptions ''Db.User)
$(deriveToJSON defaultOptions ''Db.Notification)

--Add this type of shit if compiling with a stage 1 compiler (no TemplateHaskell)
--instance T.ToJSON GenericResponse where
--   toJSON (GenericResponse e d) = T.object ["errors" .= e, "data" .= d]

--The address of the server. Necessary for email verification!
serverAddress = "http://localhost:8005"

main =
   do db <- Db.connectDb
      --Create a thread to manage the database
      forkIO $ handleDb db
      --Server configuration
      let conf = nullConf { port = 8005 }
      --Now start the actual server
      simpleHTTP conf $ handler db

bPolicy :: BodyPolicy
bPolicy = defaultBodyPolicy "/tmp/" 0 1000 1000

handler :: Connection -> ServerPart Response
handler db =
   do decodeBody bPolicy --Decode post body
      msum [ dir "register" $ register db,
             dir "verify" $ verify db,
             dir "unverify" $ unverify db,
             dir "acceptInviteEmail" $ acceptInviteEmail db,
             dir "denyInviteEmail" $ denyInviteEmail db,
             dir "leaveGroupEmail" $ leaveGroupEmail db,
             userValidatedRequests db,
             respondJson $ GenericResponse "Invalid query" ""
           ]

userValidatedRequests :: Connection -> ServerPart Response
userValidatedRequests db =
   do mId <- validate db
      let Just id = mId --Safe since it won't make it here without validate succeeding
      msum [ dir "info" $ info db id,
             dir "notifications" $ notifications db id,
             dir "groups" $ groups db id,
             dir "invitations" $ invitations db id,
             dir "createGroup" $ createGroup db id,
             dir "deleteUser" $ deleteUser db id,
             dir "updateUser" $ updateUser db id,
             --Note for this option: change how USER views note, not note itself.
             --Thus at user level.
             dir "handleNote" $ handleNote db id,
             dir "acceptInvite" $ acceptInvite db id,
             dir "denyInvite" $ denyInvite db id,
             --Move down to group-level authentication
             groupValidatedRequests db id
           ]


groupValidatedRequests :: Connection -> Db.Id -> ServerPart Response
groupValidatedRequests db uid =
   do gid <- look "gid" >>= guardRead
      gPerm <- liftIO $ Db.getGroupPermission db uid gid
      guard (gPerm /= Db.None)  --Don't let it through if user has no permission for this group
      msum [ dir "inviteUser" $ inviteUser db gid gPerm,
             dir "leaveGroup" $ leaveGroup db uid gid,
             dir "deleteGroup" $ deleteGroup db gid gPerm,
             dir "createNote" $ createNote db gid gPerm,
             dir "createAndHandleNote" $ createAndHandleNote db uid gid gPerm,
             noteValidatedRequests db uid gid gPerm
           ]

noteValidatedRequests :: Connection -> Db.Id -> Db.Id -> Db.MembershipPermission -> ServerPart Response
noteValidatedRequests db uid gid gPerm =
   do nid <- look "nid" >>= guardRead
      guard (gPerm /= Db.Member) --Let read-only users through
      msum [ dir "editNote" $ editNote db nid,
             dir "deleteNote" $ deleteNote db nid
           ]

respondJson :: (ToJSON a) => a -> ServerPart Response
respondJson = ok . toResponse . encode

register :: Connection -> ServerPart Response
register db = 
   do methodM POST  --Ensure that this is a post request
      email <- look "email"
      pass <- look "pass"
      insRes <- liftIO $ Db.insertUser db email (Just pass) Db.Pending
      let response = getRegisterResponse insRes
      respondJson response
   where getRegisterResponse (Left x)
           | "Duplicate" `isInfixOf` x =
                GenericResponse "Duplicate email! Have you forgotten your password?" ""
           | otherwise = GenericResponse x ""
         getRegisterResponse (Right _) = GenericResponse "" "Everything worked!"


verify :: Connection -> ServerPart Response
verify db =
   do uid <- look "uid" >>= guardRead
      email <- look "email"
      recChanged <- liftIO $ Db.verifyUser db uid email
      --TODO better formatted response. The end user will see this.
      case recChanged of 0 -> ok . toResponse . B.pack $ "Your account is no longer eligible for verification. Did you already select on of the links in this email?"
                         _ -> ok . toResponse . B.pack $ "Your account is now verified, allowing you to receive email notifications!"

unverify :: Connection -> ServerPart Response
unverify db =
   do uid <- look "uid" >>= guardRead
      email <- look "email"
      recChanged <- liftIO $ Db.unverifyUser db uid email
      --TODO better formatting
      case recChanged of 0 -> ok . toResponse . B.pack $ "Your account is no longer eligible for verification. Did you already select on of the links in this email?"
                         _ -> ok . toResponse . B.pack $ "Your account has been removed from our records. Sorry we bothered you!"


acceptInviteEmail :: Connection -> ServerPart Response
acceptInviteEmail db =
   do uid <- look "uid" >>= guardRead
      gid <- look "gid" >>= guardRead
      changed <- liftIO $ Db.acceptInvitation db uid gid
      case changed of 0 -> ok . toResponse . B.pack $ "Couldn't accept invite. Have you already clicked this link?"
                      _ -> do liftIO $ Db.verifyUserUid db uid --If this was a valid invitation, verify the email address using the uid alone.
                              ok . toResponse . B.pack $ "Added you to the group. You'll receive notifications from now on."


denyInviteEmail :: Connection -> ServerPart Response
denyInviteEmail db =
   do uid <- look "uid" >>= guardRead
      gid <- look "gid" >>= guardRead
      changed <- liftIO $ Db.denyInvitation db uid gid
      case changed of 0 -> ok . toResponse . B.pack $ "Couldn't deny invite. Have you already clicked this link?"
                      _ -> do liftIO $ Db.unverifyUserUid db uid
                              ok . toResponse . B.pack $ "Denied invitation. This group won't trouble you any more."

leaveGroupEmail :: Connection -> ServerPart Response
leaveGroupEmail db =
   do uid <- look "uid" >>= guardRead
      gid <- look "gid" >>= guardRead
      status <- liftIO $ Db.getUserStatus db uid
      guard (status == Db.EmailOnly) --Must be a verified email-only user to use this endpoint
      liftIO $ Db.removeMembership db uid gid
      ok . toResponse . B.pack $ "Removed you from the group in question."
      

--Attempt to log a user in and return the Uid if correct, Nothing otherwise
--Guards against invalid requests
validate :: Connection -> ServerPart (Maybe Int)
validate db =
   do email <- look "email"
      pass <- look "pass"
      isValid <- liftIO $ Db.validateUser db email pass
      guard (isJust isValid)
      return isValid

info :: Connection -> Db.Id -> ServerPart Response
info db id =
   do pending <- liftIO $ Db.getPendingNotifications db id
      handled <- liftIO $ Db.getHandledNotifications db id
      groups <- liftIO $ Db.getGroups db id
      invitations <- liftIO $ Db.getInvitations db id
      let Array pP = toJSON (pending ++ handled)
          Array pG = toJSON (groups ++ invitations)
          all = pP V.++ pG
      respondJson all

--Get all notifications relevant to a user
notifications :: Connection -> Db.Id -> ServerPart Response
notifications db id = 
   do pending <- liftIO $ Db.getPendingNotifications db id
      handled <- liftIO $ Db.getHandledNotifications db id
      let all = pending ++ handled
      respondJson all

--Get the groups to which a user belongs and the pending invitations
groups :: Connection -> Db.Id -> ServerPart Response
groups db id =
   do groups <- liftIO $ Db.getGroups db id
      respondJson groups

--Get list of invitations
invitations :: Connection -> Db.Id -> ServerPart Response
invitations db id =
   do invitations <- liftIO $ Db.getInvitations db id
      respondJson invitations

--Create group
createGroup :: Connection -> Db.Id -> ServerPart Response
createGroup db id =
   do name <- look "groupname"
      gid <- liftIO $ Db.createGroup db id name
      respondJson (Db.Membership (Db.Group gid name) Db.Admin)
      --groups db id

deleteUser :: Connection -> Db.Id -> ServerPart Response
deleteUser db id =
   do liftIO $ Db.removeUser db id
      respondJson $ GenericResponse "" "We're sorry to see you go (or are we?)"

updateUser :: Connection -> Db.Id -> ServerPart Response
updateUser db uid =
   do newEmail <- optional $ look "newEmail"
      newPass <- optional $ look "newPass"
      res <- liftIO $ Db.updateUser db uid newEmail newPass
      respondJson $ getUpdateResponse res
   where getUpdateResponse (Left msg)
           | "Duplicate" `isInfixOf` msg = GenericResponse "Email already associated with \
                                              \different account! Please choose another." ""
           | otherwise = GenericResponse msg ""
         getUpdateResponse (Right _) = GenericResponse "" "Account updated!"

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of [(x, "")] -> Just x
                              _         -> Nothing

--Exit out of the function if the read fails
guardRead :: (Read a) => String -> ServerPart a
guardRead s =
   do let result = readMaybe s
      guard (isJust result)
      let Just x = result
      return x

handleNote :: Connection -> Db.Id -> ServerPart Response
handleNote db uid =
   do nid <- look "nid" >>= guardRead
      --Don't let user's see notes they don't have access to!
      legal <- liftIO $ Db.canUserSeeNote db uid nid
      guard (legal)
      handleNoteWithNid db uid nid

--The only function that handles a note separate of the group
handleNoteWithNid :: Connection -> Db.Id -> Db.Id -> ServerPart Response
handleNoteWithNid db uid nid =
   do status <- look "newStatus" >>= guardRead
      remind <- getRemindTime status
      liftIO $ Db.insertUserNoteStatus db uid nid status remind
      --Get the note that was changed out of the database
      newNotes <- liftIO $ Db.getHandledNotification db uid nid
      --Provide it in a response. newNotes should never be null,
      --but we check anyway for safety purposes
      --(perhaps someone fucked up the database in another thread)
      if null newNotes
         then respondJson $ GenericResponse "There was no note to update! This error is highly unlikely, so you should feel excited!" ""
         else respondJson . head $ newNotes
   where getRemindTime Db.NoRemind = return Nothing
         getRemindTime Db.Hide = return Nothing
         getRemindTime _ =
            do timestamp <- look "remindAt" >>= guardRead
               return (Just timestamp)
   
acceptInvite :: Connection -> Db.Id -> ServerPart Response
acceptInvite db uid =
   do gid <- look "gid" >>= guardRead
      liftIO $ Db.acceptInvitation db uid gid
      group <- liftIO $ Db.getGroup db uid gid
      respondJson group

denyInvite :: Connection -> Db.Id -> ServerPart Response
denyInvite db uid =
   do gid <- look "gid" >>= guardRead
      rowsChanged <- liftIO $ Db.denyInvitation db uid gid
      case rowsChanged of 0 -> respondJson $ GenericResponse "No change" ""
                          _ -> respondJson $ GenericResponse "" "Invitation denied"

inviteUser :: Connection -> Db.Id -> Db.MembershipPermission -> ServerPart Response
inviteUser db gid gPerm =
   do invite <- look "invite"
      perm <- look "permissions" >>= guardRead
      guard (perm <= gPerm && perm /= Db.None) --Can't add user with greater status than you
      --Look up existence of user
      mUser <- liftIO $ Db.getUserEmail db invite
      uid <- getUid mUser invite
      case uid of Just x -> do liftIO $ Db.insertInvitation db x gid perm
                               respondJson $ GenericResponse "" "Invite sent!"
                  Nothing -> respondJson $ GenericResponse "Could not find or create user!" ""
   where getUid :: (Maybe Db.User) -> Db.Email -> ServerPart (Maybe Db.Id)
         getUid mUser invite = case mUser of Nothing -> createUser invite
                                             Just user -> return $ Just $ Db.uid user
         createUser :: Db.Email -> ServerPart (Maybe Db.Id)
         createUser invite =
            do res <- liftIO $ Db.insertUser db invite Nothing Db.UnverifiedEmailOnly
               case res of Right x -> return $ Just x
                           Left _ -> return Nothing

leaveGroup :: Connection -> Db.Id -> Db.Id -> ServerPart Response
leaveGroup db uid gid =
   do liftIO $ Db.removeMembership db uid gid
      respondJson $ GenericResponse "" "You're out! Screw those guys!"

deleteGroup :: Connection -> Db.Id -> Db.MembershipPermission -> ServerPart Response
deleteGroup db gid perm =
   do guard (perm == Db.Admin) --Must be owner to delete
      liftIO $ Db.removeGroup db gid
      respondJson $ GenericResponse "" "Group deleted"

fetchNote :: Connection -> Db.Id -> ServerPart Response
fetchNote db nid =
   do newNotes <- liftIO $ Db.getNotification db nid
      if null newNotes
         then respondJson $ GenericResponse "Note wasn't created for some stupid reason! I blame the RDBMS." ""
         else respondJson . head $ newNotes

createNote :: Connection -> Db.Id -> Db.MembershipPermission -> ServerPart Response
createNote db gid gPerm =
   do guard(gPerm == Db.Admin || gPerm == Db.Contributor)
      text <- look "text"
      time <- look "time" >>= guardRead
      nid <- liftIO $ Db.insertNotification db gid text time
      fetchNote db nid

createAndHandleNote :: Connection -> Db.Id -> Db.Id -> Db.MembershipPermission -> ServerPart Response
createAndHandleNote db uid gid gPerm =
   do guard(gPerm == Db.Admin || gPerm == Db.Contributor)
      text <- look "text"
      time <- look "time" >>= guardRead
      nid <- liftIO $ Db.insertNotification db gid text time
      handleNoteWithNid db uid nid

editNote :: Connection -> Db.Id -> ServerPart Response
editNote db nid =
   --Handle optional parameters
   do text <- optional $ look "text"
      time <- optional $ look "time" >>= guardRead
      liftIO $ Db.updateNotification db nid text time
      fetchNote db nid 

deleteNote :: Connection -> Db.Id -> ServerPart Response
deleteNote db nid =
   do liftIO $ Db.removeNotification db nid
      respondJson $ GenericResponse "" "Notification removed."

-------------------------------
-- Background Thread         --
-------------------------------

--Run background thread every 30 seconds
sleepTime = 1000 * 1000 * 30

formatTime :: (Integral a) => a -> String
formatTime = show . posixSecondsToUTCTime . realToFrac

--Send emails and remove old notifications from DB
handleDb :: Connection -> IO ()
handleDb db = forever $
   do time <- round `fmap` getPOSIXTime --Get time in seconds
      --Indicate start of pass
      putStrLn $ "Started run at " ++ (formatTime time)
      --Send pending invites to email-only users.
      sendPendingEmailOnlyInvites db
      --Now send confirmation emails to pending users.
      sendPendingVerifications db
      --Add entries to UserNoteStatus for email only users who've received new notifications
      Db.updateEmailOnlyUsers db
      --Send emails
      sendPendingEmails db time
      --Downgrade pending so emails aren't resent
      Db.downgradePendingEmails db time
      --Clear out old notifications
      Db.removeNotificationsBefore db time
      --Remove all empty groups
      Db.removeEmptyGroups db
      --Remove email-only users not in any group
      Db.removeEmptyEmailers db
      --Note finished time
      endTime <- round `fmap` getPOSIXTime
      putStrLn $ "Finished run at " ++ (formatTime endTime)
      threadDelay sleepTime

sendPendingEmailOnlyInvites :: Connection -> IO ()
sendPendingEmailOnlyInvites db = 
   do invitations <- Db.getEmailOnlyInvitations db
      let grouped = filter (not . null) . groupBy ((==) `on` Db.eUid) $ invitations
      mapM_ sendInvite grouped
   where sendInvite group =
            let title = "Pending invitations on Snys"
                to = Db.eEmail . head $ group
                body = foldr (\x acc -> (showLinks x) ++ acc) [] group
            in sendMail to title body
         showLinks (Db.EmailInvitation eUid _ eGid eName _) =
            let params = "?uid=" ++ (show eUid) ++ "&gid=" ++ (show eGid)
                acceptText = "Invited to " ++ eName ++ "!\n\tTo accept: "
                acceptLink = serverAddress ++ "/acceptInviteEmail" ++ params
                denyText = "\n\tTo deny: "
                denyLink = serverAddress ++ "/denyInviteEmail" ++ params
            in acceptText ++ acceptLink ++ denyText ++ denyLink ++ "\n\n"
                          

verificationText :: Db.Id -> Db.Email -> String
verificationText uid email = 
   "Welcome to Snys! To verify your email address, click the link below:\n\n\t"
      ++ serverAddress ++ "/verify?email=" ++ email ++ "&uid=" ++ (show uid) ++
      "\n\nIf you didn't register for Snys, click the link below to remove yourself\
      \ from our records:\n\n\t"
      ++ serverAddress ++ "/unverify?email=" ++ email ++ "&uid=" ++ (show uid)

sendPendingVerifications :: Connection -> IO ()
sendPendingVerifications db = 
   do users <- Db.getPendingUsers db
      --Downgrade pending to unverified:
      Db.downgradePending db
      mapM_ sendConfirmation users
   where sendConfirmation (Db.User uid email _ _) =
            sendMail
               email
               ("Please verify your email")
               (verificationText uid email)

sendPendingEmails :: Connection -> Db.Timestamp -> IO ()
sendPendingEmails db time = 
   do notes <- Db.getPendingEmails db time
      mapM_ send notes
   where send (Db.EmailNotification to uid uStatus from gid text time) =
            sendMail
               to
               ("Reminder from " ++ from)
               (text ++ "\n\nEvent in question occurs at " ++ (formatTime time) ++
                if uStatus == Db.EmailOnly
                   then "\n\nTo leave this group, follow this link: " ++ serverAddress ++
                        "/leaveGroupEmail?uid=" ++ (show uid) ++ "&gid=" ++ (show gid) ++
                        "\nTo delete your account, click here: " ++ serverAddress ++
                        "/unverify?email=" ++ to ++ "&uid=" ++ (show uid)
                   else ""
               )
