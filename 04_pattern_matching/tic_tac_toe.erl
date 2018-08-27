-module(tic_tac_toe).

-export([new_game/0, win/1, move/3]).


new_game() ->
    {{f, f, f},
     {f, f, f},
     {f, f, f}}.


win(GameState) ->
    case GameState of 
     {{P, _, _}, {_,P,_}, {_,_,P}} when P == x orelse P == o -> {win, P} ;
     {{_, _, P}, {_,P,_}, {P,_,_}} when P == x orelse P == o -> {win, P} ;
     {{P, P, P}, _, _} when P == x orelse P == o -> {win, P} ;
     {_, {P,P,P}, _} when P == x orelse P == o -> {win, P} ;
     {_, _, {P,P,P}} when P == x orelse P == o -> {win, P} ;
     {{P, _, _}, {P,_,_}, {P,_,_}} when P == x orelse P == o-> {win, P} ;
     {{_, P, _}, {_,P,_}, {_,P, _}} when P == x orelse P == o-> {win, P};
     {{_, _, P}, {_,_,P}, {_,_,P}} when P == x orelse P == o-> {win, P} ;
     _ -> no_win
    end.

move(Cell, Player, GameState) ->
    {error, invalid_move}.
