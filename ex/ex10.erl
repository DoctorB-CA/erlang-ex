
-module(ex10_206117756).

%% API
-export([commonFB/0]).

commonFB()->
  %%Reading the file
  {ok,File} = file:read_file("friendships.txt"),
  List = fileToList(binary:split(File,[<<"\n">>],[global])),
  %%Creation of the ETS for the MAP process
  Ets = ets:new(tab,[set]),
  buildETS(Ets,List),
  AllUser = getallUser(List),
  %%The Reduce process
  AllSub = allSubSet(AllUser,AllUser),
  ToPrint = [friends(Ets, Elm)||Elm<-AllSub],
  %%Create the output file and print to it
  FileOut = element(2,file:open("commonFB_206117756.fb",[append])),
  lists:foreach(fun([{A,B,C}])->io:format(FileOut,"~s ~s",[A,B]),[io:format(FileOut," ~s",[D])||D<-C],io:format(FileOut,"~n",[])end,ToPrint).

%%Find all mutual friends - Reduce
friends(Ets,{A,B})->
  [{_,FriendA}] = ets:lookup(Ets,A),
  [{_,FriendB}] = ets:lookup(Ets,B),
  [{A,B,FriendA--(FriendA--FriendB)}].

%%Find all subsets
allSubSet(List,[])->List;
allSubSet([_|[]],AllUser)->allSubSet(tl(AllUser),tl(AllUser));
allSubSet(User,AllUser)-> [{hd(User),hd(tl(User))}|allSubSet([hd(User)|tl(tl(User))],AllUser)].

%%Create an ETS with the elements - MAP
buildETS(Ets,[])->Ets;
buildETS(Ets,List)->
  ets:insert_new(Ets,{binary:bin_to_list(hd(hd(List))),tl(hd(List))}),buildETS(Ets,tl(List)).

%%Find all usernames
getallUser([])->[];
getallUser([<<>>])->[];
getallUser(List)->[binary:bin_to_list(hd(hd(List)))|getallUser(tl(List))].

%%Create a list from the file
fileToList([])->[];
fileToList([<<>>])->[];
fileToList(File)-> [binary:split(hd(File),[<<" ">>],[global])|fileToList(tl(File))].
