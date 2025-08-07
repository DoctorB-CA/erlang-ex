
-module(ex4_206117756).
-author("yanir").

%% API
-export([flatten/1]).
-export([smaller/2]).
-export([replace/3]).
-export([mapSub/2]).


%This function return a flat List
flatten([])->[];
flatten(List) when is_list(hd(List))-> flatten(hd(List)++tl(List));%If the head is list
flatten(List)->[hd(List)|flatten(tl(List))]. %If the head is not a list

%This function replaces all instances of Old with New in List
replace([],_,_)->[];
replace(List, Old, New) when hd(List) =:= Old-> [New|replace(tl(List),Old,New)]; %The replace
replace(List, Old, New)->[hd(List)|replace(tl(List),Old,New)].

%Returns a list of true/false atoms according to elements that are smaller or equal to Thr.
smaller(List, Thr) -> lists:map(fun(N) -> comp(N, Thr) end, List). %use lists:map

%Function for comparing numbers
comp(N, Thr) when N > Thr -> false;
comp(N, Thr) when N =< Thr -> true.


%This function subtracts elements of List1 by Arg2
mapSub(List1,[])-> List1;
mapSub(List1,Arg2) when is_number(Arg2)->lists:map(fun(N) -> sub(N, Arg2) end, List1); %use lists:map
mapSub(List1,Arg2) when is_list(Arg2)-> lenCheck(List1,Arg2).

%Functions for subtracting numbers
sub(N,Arg2)-> N-Arg2.
sub2([],[])->[];
sub2(List1,Arg2)->[(hd(List1)-hd(Arg2))|sub2(tl(List1),tl(Arg2))].

%Function to check that the lists are with the same size
lenCheck(List1,Arg2,[],[])->sub2(List1,Arg2);
lenCheck(_,_,_,[])->lenError; %Size of lists is not a same
lenCheck(_,_,[],_)->lenError; %Size of lists is not a same
lenCheck(List1,Arg2,Tl1,Tl2)-> lenCheck(List1,Arg2,tl(Tl1),tl(Tl2)).
lenCheck([],_)->lenError; %Size of lists is not a same
lenCheck(List1,Arg2)-> lenCheck(List1,Arg2,tl(List1),tl(Arg2)).