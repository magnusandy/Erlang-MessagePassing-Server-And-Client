#Erlang Chat Server
Using erlangs built-in messaging between processes to handle client and server interaction

#How To Run: 
once you have erlang installed there is a couple things that are necessary in order to get the server/client up and running
##Step 1
both client and server files will need to have their server_node() function updated to reflect your environment
```Erlang
server_node() ->
  'SNAME@NODE'.
```
##Step 2
With that complete you can start up and compile both files. 
###Server
start up an erlang environment with the SNAME you specified earlier
```
erl -sname SNAME
```
this will put you on the shell where you can compile the server code.
```
Eshell V8.1  (abort with ^G)
(SNAME@NODE)1> c(server).
{ok,server}
(SNAME@NODE)2>
```
you can go ahead and start the server using:
```
(SNAME@NODE)2> server:start_server().
```

###Client
Similarly with the client you should start another erlang shell with a different sname
```
erl -sname client1
```
then you can compile and run the client
this will put you on the shell where you can compile the server code.
```
Eshell V8.1  (abort with ^G)
(client1@NODE)1> c(client).
{ok,client}
(client@NODE)2> client:start_client().
```

Thats It! the client will now ask for a username and connect you to the server. 
type ```/help``` for commands to chat.
