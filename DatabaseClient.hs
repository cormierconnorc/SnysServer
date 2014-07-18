--Connor Cormier, 7/12/14

--TODO: Only export what's needed
module DatabaseClient where

import Database.HDBC
import Database.HDBC.ODBC
import Control.Exception
import Data.List (splitAt)
import Control.Monad (when)

--Enumerated data types in db
--Note: Pending status is for users who register via the api. PendingViaInvite is used for individuals without Snys accounts who are invited to a group. They are can only receive notifications from groups they're invited to until (if) they create an account via the Snys api. Attempting to register via the api with the email account of an EmailOnly user will result in an upgrade to verified status for that user.
data UserStatus = Verified | Unverified | EmailOnly | UnverifiedEmailOnly | Pending | PendingViaInvite | DNE deriving (Show, Read, Eq)
data MembershipPermission = Owner | Contributor | Member | None deriving (Show, Read, Eq)
data UserNoteStatus = All | JustEmail | Hide | Alarm | NoRemind deriving (Show, Read, Eq)

type Id = Int
type Timestamp = Int
type Email = String
type Password = String
type Groupname = String

--Access data types
data Group = Group { gid :: Id,
                     groupname :: Groupname
                   } deriving (Show, Read, Eq)
data Membership = Membership { group :: Group,
                               permissions :: MembershipPermission
                             } deriving (Show, Read, Eq)
data User = User { uid :: Id,
                   email :: Email,
                   password :: Password,
                   userStatus :: UserStatus
                 } deriving (Show, Read, Eq)
data Notification = Notification
                       { nid :: Id,
                         associatedGid :: Id,
                         text :: String,
                         time :: Timestamp
                       } |
                    EmailNotification
                       { sendTo :: Email,
                         fromGroup :: Groupname,
                         text :: String,
                         time :: Timestamp
                       } |
                    HandledNotification
                       { notification :: Notification,
                         status :: UserNoteStatus,
                         remindAt :: Maybe Timestamp
                       }
     deriving (Show, Read, Eq) 

databaseConnectionString :: String
databaseConnectionString = "DSN=Snys;"

connectDb :: IO Connection
connectDb = connectODBC databaseConnectionString

--Wrapper for symmetry
closeDb :: Connection -> IO ()
closeDb = disconnect

--Commonly used internal function: get id of last inserted item
retrieveId :: Connection -> IO Id
retrieveId db =
   do res <- quickQuery' db "SELECT LAST_INSERT_ID()" []
      let id = fromSql . head . head $ res
      return id

--Error handling function
sqlCatchRun :: Connection -> IO Integer -> IO (Either String Integer)
sqlCatchRun db runAction =
   do catch (do res <- runAction
                commit db
                return $ Right res
            )
            (\(SqlError _ _ msg) -> return $ Left msg)
            

-------------------------------
-- Insertion Queries         --
-------------------------------

insertUserQuery = "INSERT INTO Users(Email, Password, Status) VALUES (?, ?, ?)"

--Execute insertion query for given user information
--Fails if user with given email address already exists: Left error
--Returns Right Uid otherwise
insertUser :: Connection -> Email -> Maybe Password -> UserStatus -> IO (Either String Id)
insertUser db e p s = 
   --Attempt query, return failure if applicable
   do result <- sqlCatchRun db $ run db insertUserQuery [toSql e, toSql p, toSql $ show s]
      case result of Right _ -> do id <- retrieveId db
                                   return $ Right id
                     Left e -> return $ Left e --Must be repackaged in either with right type

insertGroupQuery = "INSERT INTO Groups(Groupname) VALUES (?)"

insertGroup :: Connection -> Groupname -> IO Int
insertGroup db name = 
   do run db insertGroupQuery [toSql name]
      commit db
      retrieveId db

insertMembershipQuery = "INSERT INTO Membership(Uid, Gid, Permissions) VALUES (?, ?, ?)"

insertMembership :: Connection -> Id -> Id -> MembershipPermission -> IO ()
insertMembership db uid gid perm = 
   do run db insertMembershipQuery [toSql uid, toSql gid, toSql $ show perm]
      commit db
                 
createGroup :: Connection -> Id -> Groupname -> IO Int
createGroup db uid name = 
   do gid <- insertGroup db name
      insertMembership db uid gid Owner
      return gid

insertNotificationQuery = "INSERT INTO Notifications(Gid, Text, Time) VALUES (?, ?, FROM_UNIXTIME(?))"

insertNotification :: Connection -> Id -> String -> Timestamp -> IO Int
insertNotification db gid text time = 
   do run db insertNotificationQuery [toSql gid, toSql text, toSql time]
      commit db
      retrieveId db

deleteUserNoteStatusQuery = "DELETE FROM UserNoteStatus WHERE Uid=? AND Nid=?"
insertUserNoteStatusQuery = "INSERT INTO UserNoteStatus(Uid, Nid, Status, RemindAt) VALUES (?, ?, ?, FROM_UNIXTIME(?))"

insertUserNoteStatus :: Connection -> Id -> Id -> UserNoteStatus -> Maybe Timestamp -> IO ()
insertUserNoteStatus db uid nid stat remind = 
   do run db deleteUserNoteStatusQuery [toSql uid,  --Delete the old value to prevent duplicates
                                        toSql nid]
      run db insertUserNoteStatusQuery [toSql uid,
                                        toSql nid,
                                        toSql $ show stat,
                                        toSql remind]
      commit db


-------------------------------
-- Removal Queries           --
-------------------------------

removeNotificationQuery = "DELETE FROM Notifications WHERE Nid = ?"

removeNotification :: Connection -> Id -> IO ()
removeNotification db nid =
   do run db removeNotificationQuery [toSql nid]
      commit db

removeUserQuery = "DELETE FROM Users WHERE Uid = ?"

removeUser :: Connection -> Id -> IO ()
removeUser db uid =
   do run db removeUserQuery [toSql uid]
      commit db

removeMembershipQuery = "DELETE FROM Membership WHERE Uid = ? AND Gid = ?"

removeMembership :: Connection -> Id -> Id -> IO ()
removeMembership db uid gid =
   do run db removeMembershipQuery [toSql uid, toSql gid]
      commit db

removeGroupQuery = "DELETE FROM Groups WHERE Gid = ?"

removeGroup :: Connection -> Id -> IO ()
removeGroup db gid =
   do run db removeGroupQuery [toSql gid]
      commit db

removeEmptyGroupsQuery = "DELETE FROM Groups WHERE NOT Gid IN (SELECT Gid FROM Membership)"

removeEmptyGroups :: Connection -> IO ()
removeEmptyGroups db =
   do run db removeEmptyGroupsQuery []
      commit db


removeNotificationsBeforeQuery = "DELETE FROM Notifications WHERE Time <= FROM_UNIXTIME(?)"

removeNotificationsBefore :: Connection -> Timestamp -> IO ()
removeNotificationsBefore db time =
   do run db removeNotificationsBeforeQuery [toSql time]
      commit db

--Note: also allow delete via unverify for email only users. This allows them to follow the same original link from their verification email to delete their account. Status is restricted so a user can't guess someone else's uid and email (however unlikely) and use it to delete their account without a password.
unverifyUserQuery = "DELETE FROM Users WHERE Uid = ? AND Email = ? \
                       \AND Status IN (\"Unverified\", \"UnverifiedEmailOnly\", \"EmailOnly\")"

unverifyUser :: Connection -> Id -> Email -> IO Integer
unverifyUser db uid email =
   do res <- run db unverifyUserQuery [toSql uid, toSql email]
      commit db
      return res

-------------------------------
-- Access Queries            --
-------------------------------

validateUserQuery = "SELECT Uid FROM Users WHERE Email = ? AND Password = ?"

--Check a user's credentials. Return "Just Uid" if correct, "Nothing" otherwise.
validateUser :: Connection -> Email -> Password -> IO (Maybe Id)
validateUser db email pass = 
   do res <- quickQuery' db validateUserQuery [toSql email, toSql pass]
      return $ fromSql `fmap` (mHead res >>= mHead)

mHead :: [a] -> Maybe a
mHead [] = Nothing
mHead (x:_) = Just x

toUser :: [SqlValue] -> User
toUser [uid, email, pass, status] = User (fromSql uid) (fromSql email)
                                       (fromSql pass) (read . fromSql $ status)

getUserQuery = "SELECT * FROM Users WHERE Uid = ?"

getUser :: Connection -> Id -> IO (Maybe User)
getUser db uid = 
   do res <- quickQuery' db getUserQuery [toSql uid]
      let user = map toUser res
      if null user
         then return Nothing
         else return $ Just (head user)
      

getUserStatusQuery = "SELECT Status FROM Users WHERE Uid = ?"

getUserStatus :: Connection -> Id -> IO UserStatus
getUserStatus db uid =
   do res <- quickQuery' db getUserStatusQuery [toSql uid]
      let perm = mHead res >>= mHead
      return $ case perm of Nothing -> DNE
                            Just x -> read $ fromSql x

getGroupsQuery = "SELECT M.Gid, Groups.Groupname, M.Permissions FROM \
                    \(SELECT * FROM Membership WHERE Uid = ?) M INNER JOIN \
                    \Groups on M.Gid = Groups.Gid"

getGroups :: Connection -> Id -> IO [Membership]
getGroups db uid =
   do res <- quickQuery' db getGroupsQuery [toSql uid]
      return $ map toMembership res
   where toMembership [gid, groupname, perm] = 
            Membership (Group (fromSql gid) (fromSql groupname)) (read . fromSql $ perm)

getGroupPermissionQuery = "SELECT Permissions FROM Membership WHERE Uid = ? AND Gid = ?"

getGroupPermission :: Connection -> Id -> Id -> IO MembershipPermission
getGroupPermission db uid gid =
   do res <- quickQuery' db getGroupPermissionQuery [toSql uid, toSql gid]
      let perm = mHead res >>= mHead
      return $ case perm of Nothing -> None
                            Just x -> read $ fromSql x

getPendingEmailsQuery = "SELECT Users.Email, Groups.Groupname, Notifications.Text, \
                           \UNIX_TIMESTAMP(Notifications.Time) FROM \
                           \UserNoteStatus INNER JOIN Users ON Users.Uid = UserNoteStatus.Uid \
                           \INNER JOIN Notifications on UserNoteStatus.Nid = Notifications.Nid \
                           \INNER JOIN Groups on Notifications.Gid = Groups.Gid \
                           \WHERE (UserNoteStatus.Status = \"All\" \
                             \OR UserNoteStatus.Status = \"JustEmail\") \
                             \AND UserNoteStatus.RemindAt <= FROM_UNIXTIME(?) \
                             \AND Users.Status IN (\"Verified\", \"EmailOnly\")"

getPendingEmails :: Connection -> Timestamp -> IO [Notification]
getPendingEmails db time =
   do res <- quickQuery' db getPendingEmailsQuery [toSql time]
      return $ map toEmailNotification res
   where toEmailNotification [e, g, t, tS] = EmailNotification (fromSql e) (fromSql g) (fromSql t) (fromSql tS)


getPendingNotificationsQuery = "SELECT Notifications.Nid, Notifications.Gid, \
                                  \Notifications.Text, UNIX_TIMESTAMP(Notifications.Time) FROM \
                                  \(SELECT Gid FROM Membership WHERE Uid = ?) M INNER JOIN \
                                  \Notifications ON M.Gid = Notifications.Gid \
                                  \WHERE Notifications.Nid NOT IN \
                                     \(SELECT Nid FROM UserNoteStatus WHERE Uid = ?)"

--Return a list of notifications that the user has not yet handled. These will appear on the client device when the app is next opened.
getPendingNotifications :: Connection -> Id -> IO [Notification]
getPendingNotifications db uid =
   do res <- quickQuery' db getPendingNotificationsQuery [toSql uid, toSql uid]
      return $ map toNotification res
   

toNotification :: [SqlValue] -> Notification
toNotification [nid, gid, text, time] =
   Notification (fromSql nid) (fromSql gid) (fromSql text) (fromSql time)

getHandledNotificationsQuery = "SELECT Notifications.Nid, Notifications.Gid, \
                                  \Notifications.Text, UNIX_TIMESTAMP(Notifications.Time), \
                                  \U.Status, UNIX_TIMESTAMP(U.RemindAt) FROM \
                                  \(SELECT * FROM UserNoteStatus WHERE Uid = ?) U INNER JOIN \
                                  \Notifications ON U.Nid = Notifications.Nid"

--Return a list of notifications that the user has already handled
getHandledNotifications :: Connection -> Id -> IO [Notification]
getHandledNotifications db uid =
   do res <- quickQuery' db getHandledNotificationsQuery [toSql uid]
      return $ map toHandledNotification res
 
toHandledNotification :: [SqlValue] -> Notification
toHandledNotification ls =
   let (gnLst, [stat, remind]) = splitAt 4 ls
       gn = toNotification gnLst
   in HandledNotification gn (read $ fromSql stat) (fromSql remind)

getHandledNotificationQuery = "SELECT N.Nid, N.Gid, N.Text, UNIX_TIMESTAMP(N.Time), \
                                 \U.Status, UNIX_TIMESTAMP(U.RemindAt) FROM \
                                 \UserNoteStatus U INNER JOIN Notifications N ON \
                                 \U.Nid = N.Nid WHERE U.Uid = ? AND N.Nid = ?"

--Get a single handled notification. Still returns list, which should be used
--for handling failure.
getHandledNotification :: Connection -> Id -> Id -> IO [Notification]
getHandledNotification db uid nid =
   map toHandledNotification `fmap`
      quickQuery' db getHandledNotificationQuery [toSql uid, toSql nid]
      
--Returns a singleton list if legal, null list otherwise
canUserSeeNoteQuery = "SELECT N.Gid FROM \
                         \(SELECT Gid FROM Membership WHERE Uid = ?) U INNER JOIN \
                         \(SELECT Gid FROM Notifications WHERE Nid = ?) N ON \
                         \U.Gid = N.Gid"

canUserSeeNote :: Connection -> Id -> Id -> IO Bool
canUserSeeNote db uid nid =
   do res <- quickQuery' db canUserSeeNoteQuery [toSql uid, toSql nid]
      return . not . null $ res


getNotificationQuery = "SELECT Nid, Gid, Text, UNIX_TIMESTAMP(Time) \
                          \FROM Notifications WHERE Nid = ?"

getNotification :: Connection -> Id -> IO [Notification]
getNotification db nid =
   map toNotification `fmap`
      quickQuery' db getNotificationQuery [toSql nid]

getPendingUsersQuery = "SELECT * FROM Users WHERE Status = \"Pending\""

getPendingUsers :: Connection -> IO [User]
getPendingUsers db =
   map toUser `fmap` quickQuery' db getPendingUsersQuery []

-------------------------------
-- Update Queries            --
-------------------------------

downgradePendingEmailsQuery = "UPDATE UserNoteStatus \
                                 \SET Status = CASE WHEN Status=\"All\" THEN \"Alarm\" \
                                 \ELSE \"NoRemind\" END \
                                 \WHERE RemindAt <= FROM_UNIXTIME(?) AND Status IN (\"All\", \"JustEmail\")"

--Move pending emails to non-email notification levels
downgradePendingEmails :: Connection -> Timestamp -> IO ()
downgradePendingEmails db time =
   do run db downgradePendingEmailsQuery [toSql time]
      commit db

updateNotification :: Connection -> Id -> (Maybe String) -> (Maybe Timestamp) -> IO ()
updateNotification db nid (Just text) (Just time) = 
   do run db "UPDATE Notifications SET Text=?,Time=FROM_UNIXTIME(?) WHERE Nid=?" [toSql text, toSql time, toSql nid]
      commit db
updateNotification db nid (Just text) _ =
   do run db "UPDATE Notifications SET Text=? WHERE Nid=?" [toSql text, toSql nid]
      commit db
updateNotification db nid _ (Just time) = 
   do run db "UPDATE Notifications SET Time=FROM_UNIXTIME(?) WHERE Nid=?" [toSql time, toSql nid]
      commit db
updateNotification db nid _ _ = return ()


verifyUserQuery = "UPDATE Users SET Status = CASE \
                     \WHEN Status=\"Unverified\" THEN \"Verified\" \
                     \ELSE \"EmailOnly\" END \
                     \WHERE Uid = ? AND Email = ? \
                     \AND Status IN (\"Unverified\", \"UnverifiedEmailOnly\")"

verifyUser :: Connection -> Id -> Email -> IO Integer
verifyUser db uid email =
   do res <- run db verifyUserQuery [toSql uid, toSql email]
      commit db
      return res


--Note: Whenever email changes, verification becomes pending. Also, uses sqlCatchRun since updates may fail (if user attempts to switch to duplicate email)
updateUser :: Connection -> Id -> Maybe Email -> Maybe Password -> IO (Either String Integer)
updateUser db uid (Just email) (Just pass) =
   do result <- sqlCatchRun db $
                   run db "UPDATE Users SET Email=?,Password=?,Status=\"Pending\" WHERE Uid=?"
                      [toSql email, toSql pass, toSql uid]
      return result
updateUser db uid _ (Just pass) =
   do changed <- run db "UPDATE Users SET Password=? WHERE Uid=?" [toSql pass, toSql uid]
      commit db
      return $ Right changed
updateUser db uid (Just email) _ =
   do result <- sqlCatchRun db $
                   run db "UPDATE Users SET Email=?,Status=\"Pending\" WHERE Uid=?"
                      [toSql email, toSql uid]
      return result
updateUser _ _ _ _ = return $ Right 0


downgradePendingQuery = "UPDATE Users SET Status=\"Unverified\" WHERE Status=\"Pending\""

downgradePending :: Connection -> IO ()
downgradePending db =
   do run db downgradePendingQuery []
      commit db

--Select unhandled notifications for email-only users.
ueouSelectQuery = "SELECT Users.Uid, Notifications.Nid, 'JustEmail', Notifications.Time FROM \
                     \Users INNER JOIN Membership ON Users.Uid = Membership.Uid \
                     \INNER JOIN Notifications ON Membership.Gid = Notifications.Gid \
                     \WHERE Users.Status = \"EmailOnly\" AND Users.Uid NOT IN (SELECT \
                     \Uid FROM UserNoteStatus WHERE Nid = Notifications.Nid)"

ueouInsertQuery = "INSERT INTO UserNoteStatus VALUES (?, ?, ?, ?)"

--Add UserNoteStatus entries for email-only users, reminding them at time of note
updateEmailOnlyUsers :: Connection -> IO ()
updateEmailOnlyUsers db = 
   do notes <- quickQuery' db ueouSelectQuery []
      --Prepare the insert statement
      insert <- prepare db ueouInsertQuery
      executeMany insert notes
      commit db
      
