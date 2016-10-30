%%%-------------------------------------------------------------------
%%% @author balegas
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Jan 2014 16:36
%%%-------------------------------------------------------------------
-module(number_generator).
-author("balegas").

%% API
-export([uniform_generator/1, zipf_generator/2, gen_sequence/2, get_probability/3]).


uniform_generator(Size) ->
  receive
    {Pid, Ref, request} ->
      Pid ! {random,Ref,rand:uniform(Size)}
  end,
  uniform_generator(Size).

zipf_generator(Size, Skew) ->
  Bottom = 1/(lists:foldl(fun(X,Sum) -> Sum+(1/math:pow(X,Skew)) end,0,lists:seq(1,Size))),
  random:seed(now()),
  zipf_generator(Size,Skew,Bottom).

zipf_generator(Size,Skew,Bottom) ->
  receive
    {Pid, Ref, request} ->
      Dice = random:uniform(),
      Next = next(Dice,Size,Skew,Bottom,0,1),
      Pid ! {random,Ref,Next},
      zipf_generator(Size,Skew,Bottom)
  end.

next(Dice,_Size,_Skew,_Bottom,Sum,CurrRank) when Sum >= Dice -> CurrRank-1;

next(Dice,Size,Skew,Bottom,Sum,CurrRank) ->
  NextRank = CurrRank +1,
  Sumi = Sum + (Bottom/math:pow(CurrRank,Skew)),
  next(Dice,Size,Skew,Bottom,Sumi,NextRank).

gen_sequence(Size,Skew) ->
  Pid = spawn_link(number_generator,zipf_generator,[Size,Skew]),
  lists:map(fun(_X)->
    Pid ! {self(),Ref=make_ref(),request},
    receive {random,Ref,Value} -> Value
    end
  end,lists:seq(1,100)).

gen_sequence(Size,Skew,SequenceSize) ->
  Bottom = 1/(lists:foldl(fun(X,Sum) -> Sum+(1/math:pow(X,Skew)) end,0,lists:seq(1,Size))),
  random:seed(now()),
  lists:map(fun(_X)->
    zipf_next(Size,Skew,Bottom)
  end,lists:seq(1,SequenceSize)).

zipf_next(Size,Skew,Bottom) ->
  Dice = random:uniform(),
  next(Dice,Size,Skew,Bottom,0,1).

get_probability(I,Size,Skew)->
  Bottom = 1/(lists:foldl(fun(X,Sum) -> Sum+(1/math:pow(X,Skew)) end,0,lists:seq(1,Size))),
  Bottom/math:pow(I,Skew).
