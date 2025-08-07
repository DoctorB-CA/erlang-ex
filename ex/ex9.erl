
-module(ex9_206117756).

%% API
-export([etsBot/0]).

%%Opening the ETS according to the required mode
etsBot()->
  {ok,File} = file:read_file("etsCommands.txt"),
  SplitFile = binary:split(File,[<<"\n">>,<<" ">>],[global]),
  Ordering = binary:bin_to_list(hd(SplitFile)),
  case Ordering of
    "set" ->executiveActions(ets:new(tab,[set]),tl(SplitFile),0,set);
    "ordered_set"->executiveActions(ets:new(tab,[ordered_set]),tl(SplitFile),0,ordered_set);
    "bag"->executiveActions(ets:new(tab,[bag]),tl(SplitFile),0,bag)
  end.

%%Open the file and write to it
executiveActions(Ets,[],_,_)-> List = ets:tab2list(Ets),File = element(2,file:open("etsRes_206117756.ets",[append])),
  lists:foreach(fun({K,V})->io:format(File,"~s ~s~n",[K,V])end,List);
executiveActions(Ets,[<<>>],_,_)-> List = ets:tab2list(Ets),File = element(2,file:open("etsRes_206117756.ets",[append])),
  lists:foreach(fun({K,V})->io:format(File,"~s ~s~n",[K,V])end,List);

executiveActions(Ets,SplitFile,LastCommand,Mode)->
  Ordering = binary:bin_to_list(hd(SplitFile)),
  case Ordering of
    %%Action selection
    "insert" ->executiveActions(Ets,tl(SplitFile),insert,Mode);
    "update"->executiveActions(Ets,tl(SplitFile),update,Mode);
    "delete"->executiveActions(Ets,tl(SplitFile),delete,Mode);
    "lookup"->executiveActions(Ets,tl(SplitFile),lookup,Mode);
    %%Insert to ETS
    Key when LastCommand==insert->
        ets:insert_new(Ets,{list_to_atom(Key),list_to_atom(binary:bin_to_list(hd(tl(SplitFile))))}),executiveActions(Ets,tl(tl(SplitFile)),insert,Mode);
    %%Update the EST
    Key when LastCommand==update->
        case Mode of
          %%Skip update in case of bag
          bag-> executiveActions(Ets,tl(tl(SplitFile)),update,Mode);
          _->ets:update_element(Ets,list_to_atom(Key),{2,list_to_atom(binary:bin_to_list(hd(tl(SplitFile))))}),executiveActions(Ets,tl(tl(SplitFile)),update,Mode)
        end;
    %%Delete from the EST
    Key when LastCommand==delete->
        ets:delete(Ets,list_to_atom(Key)),executiveActions(Ets,tl(SplitFile),delete,Mode);
    %%Print key and val from the EST
    Key when LastCommand==lookup->
        Val = ets:lookup(Ets,list_to_atom(Key)),
        case Val of
          [] -> ok;
          [{K,V}] -> io:format("key: ~s val: ~s~n",[K,V])
        end,executiveActions(Ets,tl(SplitFile),lookup,Mode)
  end.