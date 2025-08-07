
-module(ex8_206117756).

%% API
-export([startChat/1]).
-export([steadyLink/1]).
-export([steadyMon/1]).
-export([call/1]).

%%Spawns a process to evaluate function F and links the two processes
steadyLink(F)->Spawned_Pid=spawn(fun()->F()end),link(Spawned_Pid),
  receive
    _->Spawned_Pid
  after 5000 ->Spawned_Pid %%Terminates after 5 seconds
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Spawns a process to evaluate function F and monitors the spawned process
steadyMon(F)->
  {Spawned_Pid,_} = spawn_monitor(fun()->F()end),
  receive
    {_,_,_,Object,normal}->io:format("Normal termination of process ~p was detected~n",[Object]);
    {_,_,_,Object,Info}->io:format("An exception in process ~p was detected: ~p~n",[Object,Info])
    after 5000->Spawned_Pid %%Terminates after 5 seconds
  end,Spawned_Pid.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%Opening the chat in the controlled process
startChat({LocalName})->spawn(fun()->
  put(localName,LocalName),
  register(mypid,self()),
  remote_loop(0,0)end);

%%Opening the chat in the local process
startChat(RemoteName)->
  spawn(fun()->
    rpc:call(RemoteName,?MODULE,startChat,[{node()}]),
    register(mypid,self()),
    put(remoteName,RemoteName),
    local_loop(0,0)
        end).

%%The loop on the local computer
local_loop(Sent,Received)->
  receive
    %%prints the number of received/sent messages
    {toLocal,stats}->io:format("Local Stats: sent: ~p received: ~p~n",[Sent,Received+1]),local_loop(Sent,Received+1);
    %%kills the program immediately
    {toLocal,quit}-> rpc:call(get(remoteName),?MODULE,call,[{toRemote,kill}]),io:format("~p - Successfully closed.~n",[self()]),ok;
    {toLocal,kill}->io:format("~p - Successfully closed.~n",[self()]),ok;
    %%Print a message that arrived for you
    {toLocal,Msg}->io:format("~p~n",[Msg]),local_loop(Sent,Received+1);
    %%Messaging via RPC
    {rpcToRemote,Msg} -> rpc:call(get(remoteName),?MODULE,call,[{Msg}]),local_loop(Sent,Received);
    Msg->rpc:call(get(remoteName),?MODULE,call,[{toRemote,Msg}]),local_loop(Sent+1,Received)
  end.

%%The loop in the controlled computer
remote_loop(Sent,Received)->
  receive
    %%prints the number of received/sent messages
    {toRemote,stats}->io:format("Remote Stats: sent: ~p received: ~p~n",[Sent,Received+1]),remote_loop(Sent,Received+1);
    %%kills the program immediately
    {toRemote,quit}->rpc:call(get(localName),?MODULE,call,[{toLocal,kill}]),io:format("~p - Successfully closed.~n",[self()]),ok;
    {toRemote,kill}->io:format("~p - Successfully closed.~n",[self()]),ok;
    %%Print a message that arrived for you
    {toRemote,Msg} ->io:format("~p~n",[Msg]),remote_loop(Sent,Received+1);
    %%Messaging via RPC
    {fromRemote,Msg} ->rpc:call(get(localName),?MODULE,call,[{toLocal,Msg}]),remote_loop(Sent+1,Received)
  end.


call({A,B})-> mypid ! {A,B},B; %%Forwarding a message that arrived in rpc
call({Msg})-> mypid ! {fromRemote,Msg},Msg; %%Forwarding a message that arrived in rpc to the remote computer
call(Msg)->mypid ! {rpcToRemote,Msg},Msg. %%Receive a message from the user