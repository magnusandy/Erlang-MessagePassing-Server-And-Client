-module(server).
-export([start_server/0, server/1, countUsers/1, roomWatcher/1]).
%%% Change the function below to return the name of the node where the
%%% server server runs
server_node() ->
  'server@Andrews-MacBook-Pro'.
%%% Start the server
start_server() ->
  register(server, spawn(server, server, [[{"Lobby", self(), [], []}]])).

%%% This is the server process for the "server"
%%% the user list has the format [{ClientPid1, Name1},{ClientPid22, Name2},...]
server(Room_List) ->
  io:fwrite("RoomList: ~p~n", [Room_List]),
  receive
    {From, help, _} ->
      sendHelp(From),
      server(Room_List);
    {From, logon, Name} ->
      New_Room_List = server_logon(From, Name, Room_List),
      server(New_Room_List);
    {From, createRoom, Name, RoomName} ->
      New_Room_List = addRoom(From, Name, RoomName, Room_List),
      server(New_Room_List);
    {From, listRooms} ->
      listRooms(From, Room_List),
      server(Room_List);
    {From, joinRoom, ClientName, RoomName} ->
      New_Room_List = addUserToRoom(From, ClientName, RoomName, Room_List),
      server(New_Room_List);
    {From, quitServer, ClientName} ->
      New_Room_List = removeUser(From, ClientName, Room_List),
      server(New_Room_List);
    {roomUnused, RoomName} ->
      New_Room_List = cleanUpRoom(RoomName, Room_List),
      server(New_Room_List);
    {_, regularMessage, Name, Message} ->
      New_Room_List = sendMessageToRoom(Name, Message, Room_List),
      server(New_Room_List)
    end.

%%% Server adds a new user to the user list
server_logon(From, Name, Room_List) ->
  %% check if logged on anywhere else
  {LobbyRoomName, PID, Lobby_User_List, MessageList} = lists:keyfind("Lobby", 1, Room_List),
  case doesUserExist(Room_List, Name) of
    true ->
      generalMessageToClient(From, "Username is already used"),
      Room_List;
    false ->
      Count = countUsers(Room_List),
      if
        Count >= 10 ->
          From ! {server, quit, "Server is Full"},
          Room_List;
        true ->
          generalMessageToClient(From, "Login Successful"),
          generalMessageToClient(From, pastRoomMessages(MessageList)),
          NewLobbyRoom = {LobbyRoomName, PID, [{From, Name} | Lobby_User_List], MessageList},
          lists:keyreplace(LobbyRoomName, 1, Room_List, NewLobbyRoom)
      end
  end.

%checks to see if a username is already used within the system
doesUserExist([], _) ->
  false;
doesUserExist(RoomList, ClientName) ->
  [Room | Rest] = RoomList,
  {_, _, UserList, _} = Room,
  case lists:keymember(ClientName, 2, UserList) of
    true ->
      true;
    false->
      doesUserExist(Rest, ClientName)
  end.
%sends help info the the requesting client
sendHelp(RequestingClient)->
  generalMessageToClient(RequestingClient, "The following are the available commands:\n\\help: sends you help info\n\\createRoom roomName: creates a room called roomName\n\\listRooms: sends you a list of all the rooms available\n\\join roomName: moves you into the specified room\n\\leave: exits your current room to go back to the lobby\n\\quit: say goodbye to the server").

%Adds a new, empty room to the list
addRoom(CreatorPID, CreatorName, RoomName, RoomList) ->
  %checks to see if a room with that name already exists in the RoomList
  case lists:keymember(RoomName, 1, RoomList) of
    true ->
      generalMessageToClient(CreatorPID, lists:append("Error creating room, name already used: ", [RoomName])),
      RoomList;
    false ->
      io:fwrite("~s created a room called ~s~n", [CreatorName, RoomName]),
      generalMessageToClient(CreatorPID, lists:append("success creating room called: ", [RoomName])),
      PID = spawn(server, roomWatcher, [RoomName]),
      NewRoom = {RoomName, PID, [], []},
      lists:append(RoomList, [NewRoom])%Makes sure the lobby is at the head of the list
  end.

%sends a message to the client that will simply be displayed
generalMessageToClient(ClientPID, Message) ->
  ClientPID ! {server, generalMessage, Message}.

%sends the requesting client a string message of all the rooms
listRooms(ReturnPID, Room_List) ->
  RoomsMessageList = createRoomsMessage(Room_List, "Listing of Rooms:\n"),
  generalMessageToClient(ReturnPID, RoomsMessageList).

%creates a message string for the list of all room
createRoomsMessage([], Message) ->
  Message;
createRoomsMessage(RoomsList, Message) ->
  [FirstRoom | RestofRooms] = RoomsList,
  {FirstRoomName, _, _ , _} = FirstRoom,
  MessageLine = lists:append(FirstRoomName, ["\n"]),
  NewMessage = lists:append(Message, [MessageLine]),
  createRoomsMessage(RestofRooms, NewMessage).

%removes user from their current room and adds them to their desired room, returns the new roomlist
addUserToRoom(ClientPID, ClientName, RoomName, RoomList) ->
  case lists:keymember(RoomName, 1, RoomList) of
    true ->
      %room exists
      {ClientsRoomName, CliRoomPID, ClientsRoomUserList, ClientsMessageList} = findClientsRoom(RoomList, ClientName),
      NewUserList = lists:keydelete(ClientName, 2, ClientsRoomUserList), %take user out of the userlist
      RoomWithoutUser = {ClientsRoomName, CliRoomPID, NewUserList, ClientsMessageList}, %add the user back into the room
      RoomListWithoutNewRoom = lists:keydelete(ClientsRoomName, 1, RoomList),
      NewRoomList = lists:append(RoomListWithoutNewRoom, [RoomWithoutUser]),
      JoiningRoom = lists:keyfind(RoomName, 1, NewRoomList),
      {JoiningRoomName, JoiningPID, JoiningUserList, JoiningMessages} = JoiningRoom,
      NewUser =  {ClientPID, ClientName},
      NewJoiningUserList = lists:append(JoiningUserList, [NewUser]),
      NewJoiningRoom = {JoiningRoomName, JoiningPID, NewJoiningUserList, JoiningMessages},
      DeleteJoiningRoomList = lists:keydelete(JoiningRoomName, 1, NewRoomList),
      generalMessageToClient(ClientPID, lists:append("You are now in: ", [JoiningRoomName])),
      JoiningPID ! notDead, %sends the roomwatcher a message to stay alive
      generalMessageToClient(ClientPID, pastRoomMessages(JoiningMessages)),
      lists:append(DeleteJoiningRoomList, [NewJoiningRoom]);
    false ->
      %room does not exist
      generalMessageToClient(ClientPID, "The room you're trying to join does not exist"),
      RoomList %send back roomlist without changing it
  end.
%compiles a string message that can be sent to the user
pastRoomMessages(MList) ->
    pastRoomMessages(MList, "").
%recursivly creates a message of all the rooms messages for the client
pastRoomMessages([], FinalMessage) ->
   FinalMessage;
pastRoomMessages([FirstMess | Rest], FinalMessage) ->
  NewMess = lists:append([FinalMessage, FirstMess, "\n"]),
  pastRoomMessages(Rest, NewMess).

%returns the room object that the user is currently in
findClientsRoom(RoomList, ClientName) ->
  [FirstRoom | RestOfList] = RoomList,
  {_, _, FirstRoomUserList, _} = FirstRoom,
  case lists:keymember(ClientName, 2, FirstRoomUserList) of
    true -> %clientFound, return room
      FirstRoom;
    false ->
      findClientsRoom(RestOfList, ClientName)
  end.

%removes the user from their current room and sends a exit message to the client
removeUser(ClientPID, ClientName, Room_List) ->
  {ClientsRoomName, PID, UserList, Messages} = findClientsRoom(Room_List, ClientName),
  PID ! notDead, %sends the roomwatcher a message to stay alive
  LessClientList = lists:keydelete(ClientName, 2, UserList), %remove client from the room
  NewUsersRoom = {ClientsRoomName, PID, LessClientList, Messages}, %new room object without user in the  userlist
  ClientPID ! {server, quit, "Goodbye"},
  lists:keyreplace(ClientsRoomName, 1, Room_List, NewUsersRoom). %replaces the old room with the new one without the user

%sends the given message to everyone in the room with the format "CLIENTNAME says: MESSAGE"
sendMessageToRoom(ClientsName, Message, RoomList) ->
  {RName,PID, UList, MList} = findClientsRoom(RoomList, ClientsName),
  PID ! notDead, %sends the roomwatcher a message to stay alive
  FormattedMessage = lists:append([ClientsName, " says: ", Message]),
  io:fwrite("FormattedMessage: ~p~n", [FormattedMessage]),
  NewMList = lists:append(MList,[FormattedMessage]),
  messageEachUser(FormattedMessage, UList),
  lists:keyreplace(RName, 1, RoomList, {RName, PID, UList, NewMList}). %replace the old room with the new room and return

%loop through the given userList and sends them all a general message
messageEachUser(_, []) -> done;
messageEachUser(Message, [{UserPid, _ } | Rest]) ->
  generalMessageToClient(UserPid, Message),
  messageEachUser(Message, Rest).

%returns a count of total users logged into the system
countUsers(RoomList) ->
  countUsers(RoomList, 0).
countUsers([], NumUsers) -> NumUsers;
countUsers([{_, _, UList, _}| Rest], NumUsers) ->
  NewNum = NumUsers + length(UList),
  countUsers(Rest, NewNum).

%keeps the room alive if its active, if the process hasent recieved a notdead message after 7 days, the room is deleted
roomWatcher(RoomName) ->
  receive
    notDead ->
      roomWatcher(RoomName)
  after
    604800000 -> %7 days
      ServerNode = server_node(),
      {server, ServerNode} ! {roomUnused, RoomName}
  end.

%if there is any users left in the room after it itmes out, remove them and send kill message
cleanUpRoom(RoomName, RoomList) ->
  {_,_,UList,_} = lists:keyfind(RoomName, 1, RoomList),
  lists:foreach(fun({ClientPID, Name}) ->
                                        io:fwrite("killed user: ~s~n", [Name]),
                                        ClientPID ! {server, quit, "Goodbye"}
                                        end, UList),
  lists:keydelete(RoomName,1, RoomList).
