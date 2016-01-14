:- use_module(library(lists)).
:- use_module(library(random)).

board([
  [' ',' ',' ','A','A','A',' ',' ',' '],
  [' ',' ',' ',' ','A',' ',' ',' ',' '],
  [' ',' ',' ',' ','D',' ',' ',' ',' '],
  ['A',' ',' ',' ','D',' ',' ',' ','A'],
  ['A','A','D','D','K','D','D','A','A'],
  ['A',' ',' ',' ','D',' ',' ',' ','A'],
  [' ',' ',' ',' ','D',' ',' ',' ',' '],
  [' ',' ',' ',' ','A',' ',' ',' ',' '],
  [' ',' ',' ','A','A','A',' ',' ',' ']
]).

cls :- write('\e[2J').

copyList([H|T], [H|T]).

print_board(Board):-
  write('  0 1 2 3 4 5 6 7 8'),nl,
  print_lines(Board, 0).

print_lines([], _).
print_lines([H|T], I):-
  write(I), write('|'), print_chars(H), nl,
  I1 is I +1, print_lines(T, I1).

print_chars([]).
  print_chars([H|T]):-
  write(H), write('|'),
  print_chars(T).
  
print_move(StartX, StartY, TargetX, TargetY):-
  (lineSeparator, write('Moved '), write(StartX), write(':'), write(StartY), write(' to '), write(TargetX), write(':'), write(TargetY), lineSeparator).
  
print_player_turn(Player):-
 (Player = 0 -> write('It\'s the Attackers turn');
   Player = 1 -> write('It\'s the Defenders turn')), nl.

lineSeparator:-
  nl,nl,write('------------------------------'),nl,nl.

checkVictoryDefenders([H|T]):-
  member('K', H);
  (last(T, X), member('K', X));
  checkVictoryDefendersColumns(T).

checkVictoryDefendersColumns([[H|T1]|T]):-
  H = 'K';
  (last(T1, X), X = 'K');
  checkVictoryDefendersColumns(T).

checkVictoryDefendersColumns([[_|_]|[]]):-
  fail.


checkVictoryAttackers([H|T]):-
  (member('K', H), !, fail);
  checkVictoryAttackers(T).

checkVictoryAttackers([_|[]]).

checkPieceFromPlayer(Player, Piece):-
  (Player = 0, Piece = 'A');
  (Player = 1, (Piece = 'D' ; Piece = 'K')).

isKingNearThrone(X, Y):-
  (X = 4, Y = 4);
  (X = 5, Y = 4);
  (X = 3, Y = 4);
  (X = 4, Y = 3);
  (X = 4, Y = 5).

isThrone(X, Y):-
  X = 4, Y = 4.

captureKing(Board, X, Y, Result):-
  isKingNearThrone(X, Y),
  OffTop is Y -1, OffBot is Y +1,
  OffLeft is X -1, OffRight is X +1,
  nth0(Y, Board, LineKing),
  nth0(OffBot, Board, LineBottom),
  nth0(OffTop, Board, LineTop),
  nth0(OffLeft, LineKing, LeftPiece),
  nth0(OffRight, LineKing, RightPiece),
  nth0(X, LineTop, TopPiece),
  nth0(X, LineBottom, BottomPiece),
  (LeftPiece = 'A' ; isThrone(OffLeft, Y)),
  (RightPiece = 'A' ; isThrone(OffRight, Y)),
  (TopPiece = 'A' ; isThrone(X, OffTop)),
  (BottomPiece = 'A' ; isThrone(X, OffBot)),
  replace(LineKing, X, ' ', LineR),
  replace(Board, Y, LineR, Result).

capture(Board, Player, X, Y, Result):-
  OffLeft is X-1, Off2Left is X-2,
  OffRight is X+1, Off2Right is X+2,
  OffTop is Y-1, Off2Top is Y-2,
  OffBottom is Y+1, Off2Bottom is Y+2,
  /*left, right, top bottom*/
  (captureAux(Board, Player, OffLeft, Y, Off2Left, Y, R); copyList(Board, R)),
  (captureAux(R, Player, OffRight, Y, Off2Right, Y, R2); copyList(R, R2)),
  (captureAux(R2, Player, X, OffTop, X, Off2Top, R3); copyList(R2, R3)),
  (captureAux(R3, Player, X, OffBottom, X, Off2Bottom, Result); copyList(R3, Result)).

captureAux(Board, Player, X2, Y2, X3, Y3, Result):-
  switchPlayer(Player, EnemyPlayer),
  nth0(Y2, Board, Line2),
  nth0(X2, Line2, Piece2),
  checkPieceFromPlayer(EnemyPlayer, Piece2),
  ((Piece2 = 'K', captureKing(Board, X2, Y2, Result));
  (nth0(Y3, Board, Line3),
  nth0(X3, Line3, Piece3),
  (checkPieceFromPlayer(Player, Piece3); isThrone(X3, Y3)),
  replace(Line2, X2, ' ', LineResult),
  replace(Board, Y2, LineResult, Result))),
  lineSeparator, write('Captured piece at '), write(X2), write(':'), write(Y2), lineSeparator.
  
move_horizontal(Board, Player, StartX, StartY, Target, Result):-
  nth0(StartY, Board, Line),
  nth0(StartX, Line, Piece),
  checkPieceFromPlayer(Player, Piece),
  replace(Line, Target, Piece, R),
  replace(R, StartX, ' ', RLine),
  replace(Board, StartY, RLine, Result).


move_vertical(Board, Player, StartX, StartY, Target, Result):-
  nth0(StartY, Board, Line),
  nth0(StartX, Line, Piece),
  checkPieceFromPlayer(Player, Piece),
  nth0(Target, Board, LineTarget),
  replace(LineTarget, StartX, Piece, TargetR),
  replace(Line, StartX, ' ', StartR),
  replace(Board, StartY, StartR, Board2),
  replace(Board2, Target, TargetR, Result).

move(Board, Player, StartX, StartY, TargetX, TargetY, HumanPlayer, Result):-
  (StartX = TargetX, checkClearPathVertical(Board, StartX, StartY, TargetY), move_vertical(Board, Player, StartX, StartY, TargetY, Result));
  (StartY = TargetY, checkClearPathHorizontal(Board, StartX, TargetX, TargetY), move_horizontal(Board, Player, StartX, StartY, TargetX, Result));
  (HumanPlayer = Player, lineSeparator, write('Invalid move!'), lineSeparator, fail).

checkClearPathHorizontal(Board, StartX, TargetX, Y):-
  nth0(Y, Board, Line),
  ((StartX > TargetX, X is StartX -1, slice(Line, TargetX, X, R));
  (StartX < TargetX, X is StartX +1, slice(Line, X, TargetX, R))),
  checkClearPathHorizontalAux(R).

checkClearPathHorizontalAux([]).

checkClearPathHorizontalAux([H|T]):-
  H = ' ',
  checkClearPathHorizontalAux(T).

checkClearPathVertical(Board, StartX, StartY, TargetY):-
  ((StartY > TargetY, Y is StartY -1, slice(Board, TargetY, Y, R));
  (StartY < TargetY, Y is StartY +1), slice(Board, Y, TargetY, R)),
  checkClearPathVerticalAux(R, StartX).

checkClearPathVerticalAux([], _).

checkClearPathVerticalAux([H|T], X):-
  nth0(X, H, C),
  C = ' ',
  checkClearPathVerticalAux(T, X).


slice([X|_],0,0,[X]).
slice([X|Xs],0,K,[X|Ys]) :- K > 0,
   K1 is K - 1, slice(Xs,0,K1,Ys).
slice([_|Xs],I,K,Ys) :- I > 0,
   I1 is I - 1, K1 is K - 1, slice(Xs,I1,K1,Ys).



replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):-
  I > 0,
  I1 is I-1,
  replace(T, I1, X, R).


play:-
  board(X),
  playAux(X, 0).

playSP:-
  board(X),
  write('Do you want to attack or defend (0-Attack, 1-Defend):'),
  read(Player),
  playAuxSP(X, 0, Player).

playAux(Board, Player):-
  print_board(Board),
  print_player_turn(Player),
  readPiece(StartX, StartY, TargetX, TargetY),
  (move(Board, Player, StartX, StartY, TargetX, TargetY, Player, Result),
	  print_move(StartX, StartY, TargetX, TargetY),
	  capture(Result, Player, TargetX, TargetY, Result2),
	  (((Player = 0, (checkVictoryAttackers(Result2), write('Attackers won!'), !));
	   (Player = 1, (checkVictoryDefenders(Result2), write('Defenders won!'), !)));
	  (switchPlayer(Player, NewPlayer),
	   playAux(Result2, NewPlayer)))
  ; playAux(Board, Player)).
  
playAuxSP(Board, Player, HumanPlayer):-
  ((Player = HumanPlayer,
  print_board(Board),
  print_player_turn(Player),
  readPiece(StartX, StartY, TargetX, TargetY));
  (generateMove(StartX, StartY, TargetX, TargetY))),
  ((move(Board, Player, StartX, StartY, TargetX, TargetY, HumanPlayer, Result),
	  ((Player\=HumanPlayer, print_player_turn(Player));true),
	  print_move(StartX, StartY, TargetX, TargetY),
	  capture(Result, Player, TargetX, TargetY, Result2),
	  (((Player = 0, (checkVictoryAttackers(Result2), write('Attackers won!'), !));
	   (Player = 1, (checkVictoryDefenders(Result2), write('Defenders won!'), !)));
	  (switchPlayer(Player, NewPlayer),
	   playAuxSP(Result2, NewPlayer, HumanPlayer))));
  playAuxSP(Board, Player, HumanPlayer)).

generateMove(StartX, StartY, TargetX, TargetY):-
  random(0, 9, StartX),
  random(0, 9, StartY),
  random(0, 9, TargetX),
  random(0, 9, TargetY).

switchPlayer(Player, NewPlayer):-
  (Player = 0, NewPlayer is 1);
  (Player = 1, NewPlayer is 0).

 readPiece(StartX, StartY, TargetX, TargetY) :-
    write('Select a piece:'), nl,
    write('X: '), read(StartX),
    write('Y: '), read(StartY), nl,
    write('Select a destination:'), nl,
    write('X: '), read(TargetX),
    write('Y: '), read(TargetY), nl.
