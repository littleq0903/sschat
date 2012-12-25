###SSChat Server Specification

####Abstract

SSChat is a Platform-as-a-Service service which is targeting on providing PubSub real-time service with container-liked mechanism. SSChat could be scaled by opening more SSChat instances to provide more capability.

Each developer needs to register an api key on SSChat, with that api key, they can manage their users and rooms in their own isolated space, pushing and pulling real-time message according to how they scheduled users and rooms relationships.

####Project Information

Project has been open-sourced on Github: [Github - SSChat](https://github.com/littleq0903/sschat)

Powered by ChicagoBoss ([Official Site](http://chicagoboss.org/))

####Requirements:

- Erlang (R14+)
- ChicagoBoss (0.8.0+)
- cb_admin (related to ChicagoBoss, this is the admin interface plugin of it)
- jsx (already in rebar.conf)
- uuid (already in rebar.conf)

####Term

- {api_key}: string of API key (e.g. “apikey_4b76e5e2f6bd4a1987f8870d70b4da80”)
- {room_uuid}: room identity string (e.g. “room_4b76e5e2f6bd4a1987f8870d70b4da80”)
- {user_uuid}: user identity string (e.g. “user_4b76e5e2f6bd4a1987f8870d70b4da80”)
- {room_action}: “add” | “remove”
- {message}: “string”

####API Spec

#####Web API

- Register APIs: the api for registering api keys  
    + `GET /api/register/{api_key}`
    + `POST /api/register`
    + `DELETE /api/register/{api_key}`
- Room APIs: the api for managing rooms under your api key (*1)
    + `GET /api/room/{room_uuid}?apikey={api_key}`
    + `POST /api/room?apikey={api_key}`
    + `DELETE /api/room/<room_uuid>?apikey={api_key}`
    + `PUT /api/room/<room_uuid>/<user_uuid>/<room_action>?apikey={api_key}`
- User APIs: the api for managing rooms under your api key (*1)
    + `GET /api/user/<user_uuid>?apikey={api_key}`
    + `POST /api/user?apikey={api_key}`
    + `DELETE /api/user/{user_uuid}?apikey={api_key}`
	

*1: **Room** and **User** APIs must be called with the apikey parameter in its url.

##### WebSocket API
- Polling API: the location for polling and pushing messages
    - `WS /websocket/polling`
         - input data formats: JSON-encoded string
             - Login arguments
                 + command: “login”
                 + user: {user_uuid}
             - Message arguments
                 + command: “room”
                 + room: {room_uuid}
                 + msg: {message}
         - output data format: plain text
             + {message}
             + [JavaScript example](https://github.com/littleq0903/sschat/blob/master/simulation/websocket/web/index.html) 
(you can directly download and open this html file to test the behaviour of realtime actions.)

Have fun!

P.S. the data format may change in the future, if you have any suggestion just let me know or just comment in chat or comment window.

---

**SSChat: the further goals**

**New Return Data format:**

    {
        “type”: “message”,
        “data”: “<message>”,
        “from”: “<user_uuid>”,
        “to”: “room_uuid”
    }

**Presense:** able to check whether a user is online or offline
return data format: (will receive automatically when a user online/offline or manually check)

    {
        “type”: “presense”,
        “data”: “online” | “offline”,
        “from”: “<user_uuid>”
    }

more websocket command for manually checking presense:

    {
        “command”: “ping”,
        “user”: “<user_uuid>”
    }
     

**Clustering:** with the ChicagoBoss built-in clustering mechanism, made sschat could be a cluster for scaling

**MongoDB bottleneck:** because we query subscription status directly from mongodb, so we may encounter the bottleneck while scaling