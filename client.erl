-module(client).
-export([clientIn/3, printToScreen/1, clientRecieveFunction/2, start_client/0, messageServer/4]).

server_node() ->
  'server@Andrews-MacBook-Pro'.

%starts the main loops of the client, and connects to the server by sending it its pid
start_client() ->
  ServerNode = server_node(),
  Name = io:get_line("Whats your Name? "),
  [_ | ReverseName] = lists:reverse(Name),%removed newline from end of list
  NewName = lists:reverse(ReverseName),
  %creates a new client process in client_pid which is running the recieve function
  case whereis(client_pid) of
    undefined ->
      register(client_pid, spawn(client, clientRecieveFunction, [ServerNode, NewName]));
    _ -> already_logged_on
  end.

%constantly asking for input from the user
clientIn(ServerNode, Name, MyPID) ->
    %% reading whole lines from the user
    MessageLine = io:get_line(">"),
    %% Write X hellos
    messageServer(ServerNode, Name, MyPID, MessageLine),
    clientIn(ServerNode, Name, MyPID).

%takes in the lines sent from the user, if they are a command it will process and send the command if it is a message, it will send the message to the server
messageServer(_, _, _, MessageLine) when MessageLine == "\n" -> %basically do nothing if the message is just \n
  blank_message;
%processes client info and sends it to the server after parsing commands
messageServer(ServerNode, Name, MyPID, MessageLine) ->
  [_ | ReverseMessage] = lists:reverse(MessageLine),%removed newline from end of list
  Message = lists:reverse(ReverseMessage),
  CommandCharacter = string:substr(Message, 1, 1),
  [_|RestOfMessage] = Message,
  if
    CommandCharacter == "/" -> %message is a command
      Command = string:sub_word(RestOfMessage, 1),
      Arg = string:sub_word(RestOfMessage, 2),
      %here we handle all possible commands
      if
          Command == "help" ->
            {server, ServerNode} ! {MyPID, help, Name};
          Command == "createRoom" ->
            %first check if a room name was specified after the command
            if
              Arg == [] ->
                printToScreen("Room name not specified");
              Arg /= [] ->
                {server, ServerNode} ! {MyPID, createRoom, Name, Arg}
            end;
          Command == "listRooms" ->
            {server, ServerNode} ! {MyPID, listRooms};
          Command == "join" ->
            {server, ServerNode} ! {MyPID, joinRoom, Name, Arg};
          Command == "leave" ->
            {server, ServerNode} ! {MyPID, joinRoom, Name, "Lobby"};
          Command == "quit" ->
            {server, ServerNode} ! {MyPID, quitServer, Name};
          true -> %nothing else works
            printToScreen("Command not recognized")
      end;
    %if the messsage is not a command, send it as a regular message to the server
    CommandCharacter /= "/" ->
      {server, ServerNode} ! {MyPID, regularMessage, Name, Message}
  end.



%prints a string to the screen
printToScreen(StringMess) ->
   io:fwrite("~s~n", [StringMess]).

%send a logon message to the server
clientRecieveFunction(ServerNode, Name) ->
  MyPID = self(),
  {server, ServerNode} ! {MyPID, logon, Name},
  InputPID = spawn(client, clientIn, [ServerNode, Name, MyPID]), %starts the loop that gets input from the clients standard in
  clientRecieveLoop(ServerNode, InputPID). %moves to recieve return messages from the server

%wait for output from the server,
clientRecieveLoop(ServerNode, InputPID) ->
  receive
    {server, generalMessage, Message} ->
      printToScreen(Message),
      clientRecieveLoop(ServerNode, InputPID);
    {server, quit, ExitMessage} ->
      printToScreen(ExitMessage),
      exit(InputPID, kill)
  end.
