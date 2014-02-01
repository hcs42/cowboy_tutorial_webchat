Cowboy chat tutorial
--------------------

This tutorial describes creating a very simple web chat application written in
Erlang, using the Cowboy web server.

One step in this tutorial is one commit in the repository. So if any step is not
clear, you can [check the respective commit][1].

[1]: https://github.com/hcs42/cowboy_tutorial_webchat/commits/master

## I. Initial project setup

This section details how to set up a proper Erlang project. We will create an
application and a release.

If you wish to skip this test, you can clone this repository, check out the
`section-2` tag and jump to section II in this README.

### 1. Add rebar binary

We will use the `rebar` tool for building our releases, so let's download that:

```bash
$ mkdir cowboy_chat
$ cd cowboy_chat
$ wget https://raw.github.com/wiki/rebar/rebar/rebar
$ chmod +x rebar
```

### 2. Create chat application

Let's use `rebar` to generate an application skeleton for us:

```bash
$ mkdir -p apps/chat
$ cd apps/chat/
$ ../../rebar create-app appid=chat
```

### 3. Create chat release

Let's go back to the root directory of our project, create a `rel` directory and
ask `rebar` to generate a release inside `rel`:

```bash
$ mkdir rel
$ cd rel
$ ../rebar create-node nodeid=chat
$ $MY_EDITOR reltool.config
```

Let's change `lib_dirs` in line 2 from `[]` to `["../apps", "../deps"]`. We will
store our applications in `apps` and our dependencies in `deps`.

### 4. Add rebar.config

Let's create a file called `rebar.config`. `rebar` will use this file to decide
(among other things) which directories to handle and what dependencies to download.

Let's add the following content to `rebar.config`:

```erlang
{sub_dirs, [
    "apps/chat",
    "rel"
]}.

{deps, [
    {mimetypes, ".*", {git, "git://github.com/spawngrid/mimetypes.git", {tag, "1.0"}}},
    {cowboy, ".*", {git, "git://github.com/extend/cowboy", {tag, "0.8.6"}}}
]}.
```

### 5. Add cowboy apps to reltool.config

Edit `rel/reltool.config`: add the `crypto`, `mimetypes`, `ranch` and `cowboy`
applications to `sys/rel` and add them as `sys/app` entries.

### 6. Add the appmon application (and its dependencies) to reltool.config

Let's add the following applications as `sys/app` entries: `gs`,
`runtime_tools`, `appmon`. `runtime_tools` also needs to be added as a
`sys/app` entry.

## II. Try out the release

In this section, we will create a release and start it.

### 7. Fetch dependencies, compile code, generate a release

First let's download all dependencies that our project needs (based on
`rebar.config`):

```bash
$ ./rebar get-deps
```

Then compile everything:

```bash
$ ./rebar compile
```

Finally generate a release:

```bash
$ ./rebar generate
```

### 8. Try the release

Let's start the release by running the `chat` program and giving it a `console`
parameter:

```bash
$ rel/chat/bin/chat console
```

We are now in the Erlang shell. We can start the application monitor by typing

```erlang
> appmon:start().
```

We should see a graphical window. Click on the `chat` button and observe that we have 3 processes.

We can stop the console by typing CTRL-C.

Start the `chat` program without arguments and have a look at the different
actions that we can perform on it:

```bash
$ rel/chat/bin/chat
```

When we modify something in the code later and want to recompile the code,
regenerate the release and restart the chat, the following is the quickest way
to do that:

```bash
$ ./rebar compile generate skip-deps=true && rel/chat/bin/chat console
```

## III. Implement basic chat server

In this section, we will implement a basic chat server. We will do that in small
steps, so that we can test our new code at the end of each step.

### 9. Add `chat_room` `gen_server`

Let's create a `gen_server` `chat_room.erl` in `apps/chat/src`. The following is
a `gen_server` skeleton that can be just copied into the new file:

```erlang
-module(chat_room).
-behaviour(gen_server).

-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%=============================================================================
%%% API
%%%=============================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
```

We also need to add the `chat_room` server to the supervisor (which will start
and supervise it). That means that we need to add the following to the empty
list in the body of `init/1`:

```erlang
?CHILD(chat_room, worker)
```

After regenerating the release and starting `appmon`, the new `chat_room`
process will appear in the `chat` application.

### 10. Serve static webpage with Cowboy

Let's create the `apps/chat/priv/static` directory with the following files:

- `index.html`:

  ```html
  <html>
    <head>
      <meta http-equiv="Content-Type" content="text/html;charset=utf-8">
      <title>Chat room application</title>
      <script src="//ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"></script>
      <script src="/static/chat.js"></script>
    </head>
    <body>
    Hello
    </body>
  </html>
  ```

- `main.js`:

  ```javascript
  console.log("chat.js loaded");
  // or window.alert("chat.js loaded");
  ```

To ask Cowboy to serve these static files, we need to create dispatch rules and
start a Cowboy server. A good place to do that is `chat_room:init`:

```erlang
init([]) ->
    Dispatch = cowboy_router:compile([
        {'_', [

               {"/", cowboy_static,
                [{directory, {priv_dir, chat, [<<"static">>]}},
                 {file, <<"index.html">>},
                 {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]},

               {"/static/[...]", cowboy_static,
                [{directory, {priv_dir, chat, [<<"static">>]}},
                 {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]}

              ]}
    ]),

    cowboy:start_http(chat, 100,
                      [{port, 8080}],
                      [{env, [{dispatch, Dispatch}]}]),

    {ok, #state{}}.
```

In `chat_room:terminate`, we should stop the cowboy listener:

```erlang
terminate(_Reason, _State) ->
    cowboy:stop_listener(chat).
```

After compiling/regenerating/starting the release, we will be able to connect
to `http://localhost:8080` in the browser.

### 11. Add dummy websocket handler

Now that we can serve a static website, let's make it possible for clients to
connect to our server via websockets! Our websocket server won't be very smart,
it will just echo back to the client whatever it receives from it.

So let's create a Cowboy websocket handler `apps/chat/src/chat_ws_handler.erl`
that can echo the messages:

```erlang
-module(chat_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    {reply, {text, << "You said: ", Msg/binary >>}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
```

We also need to add a dispatch rule to our list of rules to let Cowboy know
when to use our handler:

```erlang
{"/ws", chat_ws_handler, []}
```

### 12. (Optional) Install Chrome web socket client and send messages to the server

If you would like to test your web socket before writing any JavaScript, you can
install the [Simple WebSocket Client][2] extension in Google Chrome, connect
to the server from that and send messages.

[2]: https://chrome.google.com/webstore/detail/simple-websocket-client/pfdhoblngboilpfeibdedpjgfnlcodoo?hl=en

### 13. Write web page that interacts with the server

Now let's implement some actual interaction between the client and the server.
That is, the HTML and the JavaScript code served by the server should contact
the server via the websocket.

So let's modify the HTML code to contain an input box, a "send" button and a
"messages" div. Modify the JavaScript so that:

- on startup it connects to the server via a websocket;
- when the "send" button is pressed, the content of the input box is sent to
  the server;
- when the server sends a message, the message it appended to the "messages"
  `div`.

If you don't want to code HTML and JavaScript now, here is a solution:

- `index.html`:

  ```html
  [...]
  <body>
    <input type="text" id="message"></input>
    <button id="send-button">Send</button>
    <div id="messages"></div>
  </body>
  [...]
  ```

- `main.js`:

  ```javascript
  var socket;
  
  function add_message(message) {
      $('#messages').append('<p></p>').children().last().text(message);
  }
  
  function read_message_input() {
      return $('#message').val();
  }
  
  function connect_to_chat() {
  
      socket = new WebSocket("ws://localhost:8080/ws");
  
      socket.onopen = function() {
          add_message("Connected.")
      };
  
      socket.onmessage = function(event) {
          add_message(event.data);
      };
  
      socket.onclose = function() {
          add_message("Connection closed.");
      };
  }
  
  function send_message(e) {
      var message = read_message_input();
      add_message(message);
      socket.send(message);
      $('#message').val("");
  }

  $(document).ready(function() {
      connect_to_chat();
      $('#send-button').click(send_message);
  })

### 14. Implement a chat server

Implement the following interface for the `chat_room` server:

- `enter(Pid)`: a new client process (which represents a user) enters the chat
  room. You can store the list of users in the chat room in the state record.
- `leave(Pid)`: a client leaves the room.
- `send_message(Pid, Message)`: send message `Message` to all client processes
  in the room except `Pid`.

Implement the followings behaviour in `chat_ws_handler`:

- When initializing and terminating, call `chat_room:enter` and
  `chat_room:leave`.
- When receiving a message from the client, call `chat_room:send_message`
  instead of sending the message back to the client.
- When receiving a message from the `chat_room` server, send that message to
  the client.

## IV. Implement improvements

The followings improvements are a good idea to implement:

- Create another text box for the nickname and show the nickname of the person
  sending a message. Handle the case when someone changes their nickname.
- Display when a client is typing into the text box to the other clients.
