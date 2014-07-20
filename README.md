#Snys Server

The server-side component of the Snys app, written in Haskell. Still in development.

##Setting up Snys

Depends on hdbc-odbc, smtps-gmail, happstack-server, and aeson packages from Hackage.
Create a `credentials.txt` file similar to the example provided to take advantage of the Snys server's email functionality.
Also, edit the server address at the top of SnysServer.hs to set the address linked to in verification emails. This solution is only temporary, but it is necessary in this version.
Uses a MySQL database accessed via ODBC. To connect, the DatabaseClient looks for a data source called "Snys".

##Snys API Documentation

Snys uses a simple JSON API to handle communication between the client and server. Descriptions of its endpoints and their required parameters follow.

####Unverified endpoints 

######/register

Register a new account with given email and password. A confirmation email will be sent by the server to verify the email address.

| Parameter | Description |
| --------- | ----------- |
| email     | Email address to register. Must not be associated with an existing account. |
| pass      | New account's password |


######/checkValid

Verify that an account with given email and password exists. Returns generic response without errs if it does, with errors if it does not.

| Parameter | Description |
| --------- | ----------- |
| email     | Email       |
| pass      | Password    |

######/verify

Verify a given email address.

| Parameter | Description |
| --------- | ----------- |
| email     | Email address to verify |
| uid       | Uid of account to verify |

######/unverify

Confirm incorrect email address. Removes user from database.

| Parameter | Description |
| --------- | ----------- |
| email     | Email address to unverify |
| uid       | Uid of account to unverify |


######/acceptInviteEmail

Accept an invite to a group as an email-only user. Also verifies email.

| Parameter | Description |
| --------- | ----------- |
| uid       | User id     |
| gid       | Group id    |

######/denyInviteEmail

Deny an invite to a group as an email-only user. Also unverifies email.

| Parameter | Description |
| --------- | ----------- |
| uid       | User id     |
| gid       | Group id    |

######/leaveGroupEmail

Leave a group via email link. Only accessible to email-only users.

| Parameter | Description |
| --------- | ----------- |
| uid       | User id     |
| gid       | gid of group to leave |

####User-verified endpoints

######/info

Retrieve all information relevant to the user: group memberships, invites, handled notifications, and pending notifications.

| Parameter | Description |
| --------- | ----------- |
| email     | Account email |
| pass      | Account password |

######/notifications

Retrieve notifications relevant to user, both handled and pending.

| Parameter | Description |
| --------- | ----------- |
| email     | Account email |
| pass      | Account password |

######/groups

Retrieve the list of groups that the user is part of.

| Parameter | Description |
| --------- | ----------- |
| email     | Account email |
| pass      | Account password |


######/invitations

Retrieve the list of groups the user has been invited to.

| Parameter | Description |
| --------- | ----------- |
| email     | Account email |
| pass      | Account password |


######/createGroup

Create a new groups with the user as the group's owner.

| Parameter | Description |
| --------- | ----------- |
| email     | Account email |
| pass      | Account password |
| groupname | A name for the new group. Does not have to be unique. |

######/deleteUser

Delete the given user account. You know, for when you realize the email you entered was wrong (please don't leave).

| Parameter | Description |
| --------- | ----------- |
| email     | Account email |
| pass      | Account password |

######/updateUser

Change information associated with the given user account.

| Parameter | Description |
| --------- | ----------- |
| email     | Account email |
| pass      | Account password |
| newEmail  | _Optional:_ New email address |
| newPass   | _Optional:_ New password |

######/handleNote

Change the status of a note. The affected note can be pending or already handled. Since this only affects the way a user sees a note and not the note itself, it requires no authentication beyond the user's account.

| Parameter | Description |
| --------- | ----------- |
| email     | Account email |
| pass      | Account password |
| nid       | ID of note to change |
| newStatus | The reminder status of the note. This should be one of `All`, `JustEmail`, `Hide`, `Alarm`, and `NoRemind`. An invalid value will cause this request to fail.
| remindAt  | The Unix timestamp (in seconds) of the time at which to remind the user. If `newStatus` is `All`, `JustEmail`, or `Alarm`, this value must be set. Otherwise, it is optional and will be ignored by the server. |

######/acceptInvite

Accept an invite to a group

| Parameter | Description |
| --------- | ----------- |
| email     | Account email |
| pass      | Account password |
| gid       | Group you were invited to |


######/denyInvite

Deny an invitation to a group

| Parameter | Description |
| --------- | ----------- |
| email     | Account email |
| pass      | Account password |
| gid       | Group you were invited to |


####Group-verified endpoints

######/inviteUser

Invite a user to the provided group. This can be done by anyone with `Member` (read only) permissions or higher.

| Parameter | Description |
| --------- | ----------- |
| email     | Account email |
| pass      | Account password |
| gid       | The id of the relevant group |
| invite    | The email address of the person to invite to the group. If they are a Snys user, they will receive an in-app notification. Otherwise, they will be sent an email notifying them (and giving them a chance to opt out) |
| permissions | The permission level to grant the new user. Must be less than or equal to the current user's group permission level | 


######/leaveGroup

Leave a group. Obviously, requires only `Member` permissions.

| Parameter | Description |
| --------- | ----------- |
| email     | Account email |
| pass      | Account password |
| gid       | The id of the relevant group |

######/deleteGroup

Delete the group in question. Requires `Owner` permission.

| Parameter | Description |
| --------- | ----------- |
| email     | Account email |
| pass      | Account password |
| gid       | The id of the relevant group |

######/createNote

Create a notification. Requires `Contributor` permissions or higher.

| Parameter | Description |
| --------- | ----------- |
| email     | Account email |
| pass      | Account password |
| gid       | The id of the relevant group |
| text      | The notification's body |
| time      | The time at which this event will occur. |

######/createAndHandleNote

Create and handle a notification with a single query.

| Parameter | Description |
| --------- | ----------- |
| email     | Account email |
| pass      | Account password |
| gid       | The id of the relevant group |
| text      | The notification's body |
| time      | The time at which this event will occur. |
| newStatus | The reminder status of the note. This should be one of `All`, `JustEmail`, `Hide`, `Alarm`, and `NoRemind`. An invalid value will cause this request to fail.
| remindAt  | The Unix timestamp (in seconds) of the time at which to remind the user. If `newStatus` is `All`, `JustEmail`, or `Alarm`, this value must be set. Otherwise, it is optional and will be ignored by the server.

####Note-verified endpoints

######/editNote

Change the text or time of a note. This will send a confirmation to all group users and remove current alarms. Shortcut for deleting and recreating the note in question. Requires `Contributor` permission or higher.

| Parameter | Description |
| --------- | ----------- |
| email     | Account email |
| pass      | Account password |
| gid       | The id of the relevant group |
| nid       | Id of note to edit |
| text      | _Optional:_ The new body of the note. Old body will be reused if this is not provided. |
| time      | _Optional:_ The new time of the note. Old time will be used if this is not provieded or valid.|

######/deleteNote

Delete a note. Requires `Contributor` permission or higher.

| Parameter | Description |
| --------- | ----------- |
| email     | Account email |
| pass      | Account password |
| gid       | The id of the relevant group |
| nid       | Id of note to delete |