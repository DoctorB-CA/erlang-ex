
-module(ex6_206117756).

%% API
-export([songList/1]).
-export([songGen/3]).

%This func return tree with songs as nodes
songList([])->digraph:new();
songList(Songs)-> songList(Songs,songList(Songs,digraph:new()),1).

%%Add vertex to the tree
songList([],G)->G;
songList(Songs,G)->digraph:add_vertex(G,hd(Songs)),songList(tl(Songs),G).

%%Add edge to the tree
songList([],G,1)->G;
songList(Songs,G,1)-> List1=[A || A <- tl(Songs) ,hd(A) =:= lists:last(hd(Songs))],List2=[A || A <- tl(Songs) ,lists:last(A) =:= hd(hd(Songs))],
  lists:foreach(fun(N)->digraph:add_edge(G,hd(Songs),N) end,List1),
  lists:foreach(fun(N)->digraph:add_edge(G,N,hd(Songs)) end,List2),
  songList(tl(Songs),G,1).


%This func return the short path from start song to the end song
songGen(G,Start,End)-> digraph:get_short_path(G,Start,End).