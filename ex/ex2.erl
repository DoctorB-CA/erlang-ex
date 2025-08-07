-module(ex2).
-export([findKelem/2]).
-export([reverse/1]).
-export([deleteKelem/2]).
-export([addKelem/3]).
-export([union/2]).


findKelem([],_) -> notFound; %didnt fount (there is no elment in the list)
findKelem(List,1) -> hd(List); %we find the elemnet
findKelem(List,K) -> findKelem(tl(List),K-1). %Search in recursion the K element.

%This function deletes all instances of Elem from List.
deleteKelem(List,Elem) -> [A || A <- List ,A =/= Elem ].

%This function adds Elem to List in K place.
addKelem(List,1,Elem) -> [Elem|List]; %Stop condition.
addKelem(List,K,Elem) -> [hd(List)|addKelem(tl(List),K-1,Elem)]. %Search in recursion the K place.

%This function returns the union of these two lists and removes multiple instances from both lists.
union([],[]) -> []; %Stop condition.
union([],List2) -> [hd(List2)|union([],[C || C <- tl(List2), C=/= hd(List2)])]; %Delaete duplicates in list 2 and makes the union.
union(List1,List2) -> [hd(List1)|union([A || A <- tl(List1), A=/= hd(List1)],[B || B <- List2 ,B =/= hd(List1) ])]. %Delaete duplicates in list 1.

%This function reverse List’s items.
reverse(List) -> reverse(List, []). %First call.
reverse([],List) ->  List; %Stop condition.
reverse(List1, List2) -> reverse(tl(List1), [hd(List1)|List2]). %Put the head of list1 in head of list2.
