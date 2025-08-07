
-module(ex7_206117756).

%% API
-export([steady/1]).
-export([calc/3]).

%%A function that tests another function
%%Running example: ex7:206117756:steady(fun ex7_206117756:f/0)
steady(F)->
  process_flag(trap_exit,true),
  Mylog = element(2,file:open("myLog_206117756.elog",[append])), %%Our file
  try F() of
    %%Printing in case everything is fine
    Result->io:format(Mylog,"{~p, success, ~p}~n",[os:system_time(),Result])
  catch
    %%Printing in case of error
    error:Error ->io:format(Mylog,"{~p, error, ~p}~n",[os:system_time(),Error]);
    exit:Exitreturns ->io:format(Mylog,"{~p, exit, ~p}~n",[os:system_time(),Exitreturns]);
    throw:Throw ->io:format(Mylog,"{~p, throw, ~p}~n",[os:system_time(),Throw])
  end.

%A function that divides between A and B.
calc(division,A,B)->
  try A/B of
    Result -> Result %%If everything is fine
  catch
    error:badarith-> divide_by_zero_error %%Error of division by 0
  end.