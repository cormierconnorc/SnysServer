--Connor Cormier, 7/12/14

--TODO: Only export what's needed
module DatabaseClient where

import Database.HDBC
import Database.HDBC.ODBC
import Control.Exception
import Data.List (splitAt)
import Control.Monad (when)

--Enumerated data types in db
data UserStatus = Verified | Unverified | OptOut | DNE deriving (Show, Read, Eq)
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
                         text :: String
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
   do result <- catch (do res <- run db insertUserQuery [toSql e, toSql p, toSql $ show s]
                          commit db
                          return $ Right res
                      )
                      (\(SqlError _ _ msg) -> return $ Left msg)
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

insertNotificationQuery = "INSERT INTO Notifications(Gid, Text, Time) VALUES (?, ?, FROM_UNIXTIME(w?))"

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

getPendingEmailsQuery = "SELECT Users.Email, Groups.Groupname, Notifications.Text FROM \
                           \UserNoteStatus INNER JOIN Users ON Users.Uid = UserNoteStatus.Uid \
                           \INNER JOIN Notifications on UserNoteStatus.Nid = Notifications.Nid \
                           \INNER JOIN Groups on Notifications.Gid = Groups.Gid \
                           \WHERE (UserNoteStatus.Status = \"All\" \
                             \OR UserNoteStatus.Status = \"JustEmail\") \
                             \AND UserNoteStatus.RemindAt <= FROM_UNIXTIME(?)"

getPendingEmails :: Connection -> Timestamp -> IO [Notification]
getPendingEmails db time =
   do res <- quickQuery' db getPendingEmailsQuery [toSql time]
      return $ map toEmailNotification res
   where toEmailNotification [e, g, t] = EmailNotification (fromSql e) (fromSql g) (fromSql t)


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


-------------------------------
-- Update Queries            --
-------------------------------

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
