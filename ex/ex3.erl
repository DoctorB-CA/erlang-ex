
-module(ex3_206117756).
-author("yanir").

%% API
-export([sortRes/2]).
-export([sortResLC/1]).
-export([sortResPM/1]).
-export([sortResLM/1]).
-export([mSort/1]).
-export([qSort/1]).
-export([matElemMult/2]).
-export([filter_g/2]).
-export([filter_p/2]).
-export([even/1]).
-export([fiboR/1]).
-export([fiboT/1]).


%Returns a list of all even members of List, in the same order as appeared in it
even([]) -> [];
even(List) when (hd(List) rem 2 =/= 0) -> even(tl(List));
even(List) when (hd(List) rem 2 =/= 1) -> [hd(List)|even(tl(List))].


%If Filter is the atom ‘numbers’ than remove all numbers from list
%If Filter is the atom ‘atoms’ than remove all atoms from list
%Implement it using Guards
filter_g([],_) -> [];
filter_g(List,Filter) when is_atom(hd(List)),Filter =:= atoms -> filter_g(tl(List),Filter);
filter_g(List,Filter) when is_number(hd(List)),Filter =:= numbers -> filter_g(tl(List),Filter);
filter_g(List,Filter) when is_atom(hd(List)),Filter =:= numbers -> [hd(List)|filter_g(tl(List),Filter)];
filter_g(List,Filter) when is_number(hd(List)),Filter =:= atoms -> [hd(List)|filter_g(tl(List),Filter)].


%Sorts List according to the residue of division by 3 with List comprehension
sortResLC(List)-> Zero=[A || A <- List ,A rem 3 =:= 0],
  One=[B || B <- List ,B rem 3 =:= 1],
  Two=[C || C <- List ,C rem 3 =:= 2],
  qSort(Zero)++qSort(One)++qSort(Two).

%Returns a sorted List. Implement as quick sort.
qSort([]) -> [];
qSort([Pivot | List]) -> %Select the head as pivot.
  Less = [A || A <- List, A =< Pivot],
  Greater = [B || B <- List, B > Pivot],
  qSort(Less) ++ [Pivot] ++ qSort(Greater).


%Returns the N’th Fibonacci number. Implement it using a recursion
fiboR(1)->1;
fiboR(2)->1;
fiboR(N)-> fiboR(N-1)+fiboR(N-2).

%Returns the N’th Fibonacci number. Implement it using a tail recursion
fiboT(1)->1;
fiboT(2)->1;
fiboT(N)->fiboT(N-2,1,1).
fiboT(0,_,B)->B;
fiboT(N,A,B)->fiboT(N-1,B,A+B).
%fibonacci tail was faster.


%Multiply matrix element by element.
matElemMult([],[]) -> [];
matElemMult(MatA,MatB) -> [mulVec(hd(MatA),hd(MatB))|matElemMult(tl(MatA),tl(MatB))].

%This function multiplies between vector
mulVec([],[])->[];
mulVec(ListA,ListB)->[float(hd(ListA)*hd(ListB))|mulVec(tl(ListA),tl(ListB))].


%Sorts List according to the residue of division by 3 with lists module
sortResLM(List)-> lists:sort(fun residueThree/2,List).

%This function determines how to sort (By residue 3 and after by remainder).
residueThree(A,B)-> if
                      (A rem 3) < (B rem 3) -> true;
                      (A rem 3) > (B rem 3) -> false;
                      A < B -> true;
                      A > B -> false
                    end.


%If Filter is the atom ‘numbers’ than remove all numbers from list
%If Filter is the atom ‘atoms’ than remove all atoms from list
%Implement it using pattern matching
filter_p(List,numbers)-> [A || A <- List, is_atom(A)];
filter_p(List,atoms)-> [A || A <- List, is_number(A)].

%Sorts List according to the residue of division by 3
sortRes(List,lc)-> sortResLC(List); %with List comprehension
sortRes(List,pm)-> sortResPM(List); %with Pattern matching
sortRes(List,lm)-> sortResLM(List). %with lists module

%Sorts List according to the residue of division by 3 with Pattern matching
sortResPM([])->[];
sortResPM([H|T])->qSort(sortResPM([H|T],0)) ++ qSort(sortResPM([H|T],1)) ++ qSort(sortResPM([H|T],2)).
sortResPM([],_)->[];
sortResPM([H|T],Num) when (H rem 3) == Num -> [H|sortResPM(T,Num)];
sortResPM([_|T],Num) -> sortResPM(T,Num).

%Returns a sorted List. Implement as merge sort.
mSort([]) ->[];
mSort([List]) -> [List];
mSort(List)   ->  {ListA,ListB} = lists:split(length(List) div 2, List), %Separation stage
  merge(mSort(ListA), mSort(ListB)). %marger stage

%This function merges the separated list from mSort function.
merge(ListA, ListB)    -> merge(ListA, ListB, []).
merge([], ListB, A) -> A++ListB;
merge(ListA, [], A) -> A++ListA;
merge(ListA, ListB, A) when hd(ListB)>=hd(ListA) -> merge(tl(ListA), ListB, A++[hd(ListA)]);
merge(ListA, ListB, A) when hd(ListB)<hd(ListA)  -> merge(ListA, tl(ListB), A++[hd(ListB)]).