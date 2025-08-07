
-module(exm).

%% API
-export([exp_to_bdd/3]).
-export([solve_bdd/2]).
-export([listOfLeaves/1]).
-export([reverseIteration/1]).


-record(node,{num,l,r}).
-record(leaf,{num,l,r,route}).


%--------------------- Main functions---------------------%

%---- exp_to_bdd ------------- %
%Return BDD according to selected type and order
exp_to_bdd(BoolFunc,Order,Type)->
  TimeOn=erlang:timestamp(),
  case Type of
  record ->BDD = order(Order,[reduction(B)||B<-[bddRecord(A,BoolFunc,#{})||A <- permutations(findVar(BoolFunc))]]);
  map->BDD = order(Order,[bddMap(A,BoolFunc,#{},#{})||A <- permutations(findVar(BoolFunc))])
  end,
  TimeOff=erlang:timestamp(),
  io:format("RunningTime (exp_to_bdd)= ~p  microseconds~n",[timer:now_diff(TimeOff,TimeOn)]),
  BDD.
%---------------------- %

%---- solve_bdd ------------- %
%Return the time of solve_bdd function
solve_bdd(BDD,List)->
  TimeOn=erlang:timestamp(),
  Sol = solve_bdd(BDD,List,1),
  TimeOff=erlang:timestamp(),
  io:format("RunningTime (solve_bdd)= ~p  microseconds~n",[timer:now_diff(TimeOff,TimeOn)]),
  Sol.
%Solve bdd (record type)
solve_bdd(#node{num=Num,l=L,r=R},List,1)->
  Val = find_value(Num,List),
  case Val of
    true->solve_bdd(L,List,1);
    1->solve_bdd(L,List,1);
    false->solve_bdd(R,List,1);
    0->solve_bdd(R,List,1)
  end;
%Solve bdd (map type)
solve_bdd(Map,List,1) when is_map(Map)->
  Val = find_value(maps:get(val,Map),List),
  case Val of
    true->solve_bdd(maps:get(l,Map),List,1);
    1->solve_bdd(maps:get(l,Map),List,1);
    false->solve_bdd(maps:get(r,Map,1),List);
    0->solve_bdd(maps:get(r,Map,1),List)
  end;
solve_bdd(Arg,_,1)->Arg.
%---------------------- %

%---- listOfLeaves ------------- %
%Return list of leaves for record type
listOfLeaves(#node{num=Num,l=L,r=R})->
  TimeOn=erlang:timestamp(),
  Check = (((L=:=1) or (L=:=0) or (L=:=true) or (L=:=false)) and ((R=:=1) or (R=:=0) or (R=:=true) or (R=:=false))),
  case Check of
    false->Sol=listOfLeaves(L,[Num])++listOfLeaves(R,[Num]);
    true->Sol=[#leaf{num=Num,l=L,r=R,route=[]}]
  end,
  TimeOff=erlang:timestamp(),
  io:format("RunningTime (listOfLeaves)= ~p  microseconds~n",[timer:now_diff(TimeOff,TimeOn)]),
  Sol;
%Return list of leaves for map type
listOfLeaves(Map) when is_map(Map)->
  TimeOn=erlang:timestamp(),
  Check = (((maps:get(l,Map)=:=1) or (maps:get(l,Map)=:=0) or (maps:get(l,Map)=:=true) or (maps:get(l,Map)=:=false))
    and ((maps:get(r,Map)=:=1) or (maps:get(r,Map)=:=0) or (maps:get(r,Map)=:=true) or (maps:get(r,Map)=:=false))),
  case Check of
    false->Sol=listOfLeaves(maps:get(l,Map),[maps:get(val,Map)])++listOfLeaves(maps:get(r,Map),[maps:get(val,Map)]);
    true->Sol=[#leaf{num=maps:get(val,Map),l=maps:get(l,Map),r=maps:get(r,Map),route=[]}]
  end,
  TimeOff=erlang:timestamp(),
  io:format("RunningTime (listOfLeaves)= ~p  microseconds~n",[timer:now_diff(TimeOff,TimeOn)]),
  Sol;
listOfLeaves(_)->[].
%Return list of leaves for record type
listOfLeaves(#node{num=Num,l=L,r=R},List)->
  Check = (((L=:=1) or (L=:=0) or (L=:=true) or (L=:=false)) and ((R=:=1) or (R=:=0) or (R=:=true) or (R=:=false))),
  case Check of
    false->listOfLeaves(L,[Num|List])++listOfLeaves(R,[Num|List]);
    true->[#leaf{num=Num,l=L,r=R,route=List}]
  end;
listOfLeaves(Map,List) when is_map(Map)->
  Check = (((maps:get(l,Map)=:=1) or (maps:get(l,Map)=:=0) or (maps:get(l,Map)=:=true) or (maps:get(l,Map)=:=false))
    and ((maps:get(r,Map)=:=1) or (maps:get(r,Map)=:=0) or (maps:get(r,Map)=:=true) or (maps:get(r,Map)=:=false))),
  case Check of
    false->listOfLeaves(maps:get(l,Map),[maps:get(val,Map)|List])++listOfLeaves(maps:get(r,Map),[maps:get(val,Map)|List]);
    true->[#leaf{num=maps:get(val,Map),l=maps:get(l,Map),r=maps:get(r,Map),route=List}]
  end;
listOfLeaves(_,_)->[].
%---------------------- %

%---- reverseIteration ------------- %
%Return route to the root
reverseIteration(#leaf{num=_,l=_,r=_,route=List})->
  TimeOn=erlang:timestamp(),
  TimeOff=erlang:timestamp(),
  io:format("RunningTime (reverseIteration)= ~p  microseconds~n",[timer:now_diff(TimeOff,TimeOn)]),
  List.


%--------------------- Sub functions---------------------%



%Return the tree according to the selected order
order(Order,ListTrees)->
  case Order of
    num_of_nodes-> numOfnodes(hd(ListTrees),numOfnodes(hd(ListTrees)),tl(ListTrees));
    tree_height-> treeHeight(hd(ListTrees),treeHeight(hd(ListTrees)),tl(ListTrees));
    num_of_leafs-> numOfleafs(hd(ListTrees),numOfleafs(hd(ListTrees)),tl(ListTrees))
  end.










%Create and reduction BDD with map
bddMap([],BoolFunc,_,Val)-> solver(BoolFunc,Val);
bddMap(List,BoolFunc,Map,Val)->Maps= Map#{val=>hd(List),l=>bddMap(tl(List),BoolFunc,Map,Val#{hd(List)=>1}),r=>bddMap(tl(List),BoolFunc,Map,Val#{hd(List)=>0})},
  case (maps:get(l,Maps) =:= maps:get(r,Maps)) of
    true->maps:get(l,Maps);
    false->Maps
  end.



%Create BDD with record
bddRecord([],BoolFunc,Map)-> solver(BoolFunc,Map);
bddRecord(List,BoolFunc,Map)-> #node{num=hd(List),l=bddRecord(tl(List),BoolFunc,Map#{hd(List)=>1}),r=bddRecord(tl(List),BoolFunc,Map#{hd(List)=>0})}.

%Applying the reduction rules
reduction(#node{num=Num,l=L,r=R})->
  case L =:= R of
    false -> #node{num=Num,l=reduction(L),r=reduction(R)};
    true-> reduction(R)
  end;
reduction(Num)->Num.



%Return the tree with the minimum leafs
numOfleafs(MinTree,_,[])->MinTree;
numOfleafs(MinTree,MinSize,ListTrees)->
  Hight = numOfleafs(hd(ListTrees)),
  case Hight<MinSize of
    true->numOfleafs(hd(ListTrees),Hight,tl(ListTrees));
    false->numOfleafs(MinTree,MinSize,tl(ListTrees))
  end.
numOfleafs(#node{num=_,l=L,r=R})->  numOfleafs(L)+numOfleafs(R);
numOfleafs(Map) when is_map(Map)-> (numOfleafs(maps:get(l,Map)))+(numOfleafs(maps:get(r,Map)));
numOfleafs(_)->1.


%Return the tree with the minimum nodes
numOfnodes(MinTree,_,[])->MinTree;
numOfnodes(MinTree,MinSize,ListTrees)->
  Hight = numOfnodes(hd(ListTrees)),
  case Hight<MinSize of
    true->numOfnodes(hd(ListTrees),Hight,tl(ListTrees));
    false->numOfnodes(MinTree,MinSize,tl(ListTrees))
  end.
numOfnodes(#node{num=_,l=L,r=R})-> 1+numOfnodes(L)+numOfnodes(R);
numOfnodes(Map) when is_map(Map)-> 1+(numOfnodes(maps:get(l,Map)))+(numOfnodes(maps:get(r,Map)));
numOfnodes(_)->0.


%Return the tree with the lowest height:
treeHeight(MinTree,_,[])->MinTree;
treeHeight(MinTree,MinSize,ListTrees)->
  Hight = treeHeight(hd(ListTrees)),
  case Hight<MinSize of
    true->treeHeight(hd(ListTrees),Hight,tl(ListTrees));
    false->treeHeight(MinTree,MinSize,tl(ListTrees))
  end.
treeHeight(#node{num=_,l=L,r=R})->max(1+treeHeight(L),1+treeHeight(R));
treeHeight(Map) when is_map(Map)-> max(1+(treeHeight(maps:get(l,Map))),1+(treeHeight(maps:get(r,Map))));
treeHeight(_)->0.


%Find the all variable
findVar({'not',Arg})->  findVar(Arg);
findVar({_,{Arg1,Arg2}}) ->  union(findVar(Arg1),findVar(Arg2));
findVar(Arg)-> [Arg].

%Solve BoolFunction with map
solver({'not',Arg},Tuple)->  not solver(Arg,Tuple);
solver({'or',{Arg1,Arg2}},Tuple)->  solver(Arg1,Tuple) or solver(Arg2,Tuple);
solver({'and',{Arg1,Arg2}},Tuple)->  solver(Arg1,Tuple) and solver(Arg2,Tuple);
solver(_,true)-> true;
solver(_,1)-> true;
solver(_,false)-> false;
solver(_,0)-> false;
solver(Arg,Tuple)-> #{Arg := Value}=Tuple,solver(Arg,Value).


%This function returns the union of these two lists and removes multiple instances from both lists.
union([],[]) -> []; %Stop condition.
union([],List2) -> [hd(List2)|union([],[C || C <- tl(List2), C=/= hd(List2)])]; %Delete duplicates in list 2 and makes the union.
union(List1,List2) -> [hd(List1)|union([A || A <- tl(List1), A=/= hd(List1)],[B || B <- List2 ,B =/= hd(List1) ])]. %Delete duplicates in list 1.

%generates all permutations of the elements in a list:
permutations([]) -> [[]];
permutations(L)  -> [[H|T] || H <- L, T <- permutations(L--[H])].


%Find value in list of tuples:
find_value(Key, List) ->
  case lists:keyfind(Key, 1, List) of
    {Key, Result} -> Result
  end.
