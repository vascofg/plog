:-use_module(library(clpfd)).
:-use_module(library(lists)).
:-use_module(library(random)).

barrel(_,[_,_],[_,_]).

board([
  [barrel(_,[_,4],[_,_]),barrel(12,[_,_],[_,_]),barrel(_,[2,_],[_,_])],
  [barrel(_,[6,_],[_,_]),barrel(_,[_,_],[_,2]),barrel(13,[_,_],[_,_])],
  [barrel(14,[_,_],[_,_]),barrel(_,[_,_],[3,_]),barrel(48,[_,1],[_,_])]]).
  
%Gera board aleatório com tamanho Size*Size
board_empty(Board,Size):-
	board_empty(Board,Size,0).

board_empty(_,Size,Size).
board_empty([Line|T],Size,Count):-
	board_empty_line(Line,Size,0),
	Count1 is Count+1,
	board_empty(T,Size,Count1).

board_empty_line(_,Size,Size).
board_empty_line([barrel(_,[_,_],[_,_])|T],Size,Count):-
	Count1 is Count+1,
	board_empty_line(T,Size,Count1).

board2([
  [barrel(_,[_,4],[_,_]),barrel(12,[_,_],[_,_]),barrel(_,[2,_],[_,_]),barrel(_,[_,_],[_,_])],
  [barrel(_,[6,_],[_,_]),barrel(_,[_,_],[_,2]),barrel(13,[_,_],[_,_]),barrel(_,[_,_],[_,_])],
  [barrel(14,[_,_],[_,_]),barrel(_,[_,_],[3,_]),barrel(48,[_,1],[_,_]),barrel(_,[_,_],[_,_])],
  [barrel(_,[_,_],[_,3]),barrel(24,[_,_],[_,_]),barrel(_,[_,3],[_,_]),barrel(_,[_,6],[_,_])]
  ]).

go:-
  board(X),restrict(X),getFirstSolution(X),printBoard(X).

go_random(Size):-
  board_empty(X,Size),restrict(X),getRandomSolution(X),printBoard(X).

go_random(N,Size):-
  go_random(N,0,Size).

go_random(N,N,_).
go_random(N,Count,Size):-
  go_random(Size),Count1 is Count+1,go_random(N,Count1,Size).

restrict(Board):-
  length(Board,Len),
  MaxDomain is Len*2,
  restrictTops(Board),
  restrictLineCols(Board,MaxDomain,1),
  getAllBarrelCols(Board,Cols),
  restrictLineCols(Cols,MaxDomain,2),
  restrictBarrels(Board).

getFirstSolution(Board):-
  getAllElems(Board,Elems),
  labeling([ff],Elems).  
  
getRandomSolution(Board):-
  getAllElems(Board,Elems),
  labeling([ff,value(mySelValores)],Elems).
  
%%http://paginas.fe.up.pt/~eol/LP/0809/documents/Praticas/PLR/SelVariaveisValores.pl 
%%Heuristica de Seleccao de Valores - Valor aleatório
mySelValores(Var, _Rest, BB, BB1) :-
	fd_set(Var, Set),
	fdset_to_list(Set, _List),
	random(1, 100, Value),
	(   
	   first_bound(BB, BB1), Var #= Value
			;   
	   later_bound(BB, BB1), Var #\= Value
       ).


restrictTops(Board):-
  getTops(Board, Tops),
  domain(Tops,4,91),
  all_different(Tops).

restrictLineCols([],_,_).
restrictLineCols([H|T],MaxDomain,LC):-
  getLineCol(H,FirstLineCol,1,LC),
  domain(FirstLineCol,1,MaxDomain),
  all_different(FirstLineCol),
  getLineCol(H,SecondLineCol,2,LC),
  domain(SecondLineCol,1,MaxDomain),
  all_different(SecondLineCol),
  restrictLineCols(T,MaxDomain,LC).

restrictBarrels([]).
restrictBarrels([[]|T]):-
  restrictBarrels(T).
restrictBarrels([[barrel(Top,[X1,X2],[X3,X4])|R1]|T]):-
  (Top #= X1*X2*X3*X4 #\/ Top #= X1+X2+X3+X4),
  restrictBarrels([R1|T]).

getAllElems([],[]).
getAllElems([[]|T],Elems):-
  getAllElems(T,Elems).
getAllElems([[Barrel|R1]|T],[Top,X1,X2,X3,X4|R]):-
  getElems(Barrel,Top,X1,X2,X3,X4),
  getAllElems([R1|T],R).

getElems(barrel(Top,[X1,X2],[X3,X4]),Top,X1,X2,X3,X4).

getTops([],[]).
getTops([[]|T],Tops):-
  getTops(T,Tops).
getTops([[Barrel|R1]|T],[Top|R]):-
  getElems(Barrel,Top,_,_,_,_),
  getTops([R1|T],R).

getLineCol([],[],_,_).
getLineCol([Barrel|T],[X1,X2|R],N,LC):-
  ((LC=1,
   ((N = 1, getElems(Barrel,_,X1,X2,_,_));
   (N = 2, getElems(Barrel,_,_,_,X1,X2))));
  (LC=2,
   ((N = 1, getElems(Barrel,_,X1,_,X2,_));
   (N = 2, getElems(Barrel,_,_,X1,_,X2))))),
  getLineCol(T,R,N,LC).

getAllBarrelCols(Board,BarrelCols):-
  length(Board,NumLines),
  getAllBarrelColsAux(Board,NumLines,0,BarrelCols).

getAllBarrelColsAux(_,NumCols,NumCols,_).
getAllBarrelColsAux(Board,NumCols,Count,[H|T]):-
  getBarrelCol(Board,Count,H),
  Count1 is Count+1,
  getAllBarrelColsAux(Board,NumCols,Count1,T).

getBarrelCol([],_,[]).
getBarrelCol([Line|R],N,[Barrel|T]):-
  nth0(N,Line,Barrel),
  getBarrelCol(R,N,T).

/* ------------------ PRINT PREDICATES ------------------ */

printBoard([]):-nl,write('==================================================='),nl,nl.
printBoard([Line|R]):-
  length(Line, LineSize),
  printBoard(Line,1,LineSize),
  printBoard(R).

printBoard(_,12,_).
printBoard(Line,Count,LineSize):-
  Count1 is Count+1,
  printBoardAux(Line, LineSize, 0, Count),
  printBoard(Line,Count1,LineSize),!.

printBoardAux(_, LineSize, LineSize, _):-
  nl.
printBoardAux([H|T], LineSize, Count, Line):-
  Count1 is Count+1,
  printBarrel(H,Line),
  printBoardAux(T, LineSize, Count1, Line).

printBarrel(barrel(_,_,_),1):-
  write('   _.........._  ').
printBarrel(barrel(Top,_,_),2):-
  write(' .´     '),printLeadingZero(Top),write('     `.').
printBarrel(barrel(_,_,_),3):-
  write(' |`-==========-´|').
printBarrel(barrel(_,_,_),4):-
  write(' |      ||      |').
printBarrel(barrel(_,[X1,X2],_),5):-
  write(' |   '),write(X1), write('  ||  '),write(X2),write('   |').
printBarrel(barrel(_,_,_),6):-
  write(' |      ||      |').
printBarrel(barrel(_,_,_),7):-
  write(' |==============|').
printBarrel(barrel(_,_,_),8):-
  write(' |      ||      |').
printBarrel(barrel(_,_,[X3,X4]),9):-
  write(' |   '),write(X3), write('  ||  '),write(X4),write('   |').
printBarrel(barrel(_,_,_),10):-
  write(' \\      ||      /').
printBarrel(barrel(_,_,_),11):-
  write('  `------------´ ').

/*
   _.........._
 .´     16     `.
 |`-==========-´|
 |      ||      |
 |   5  ||  4   |
 |      ||      |
 |==============|
 |      ||      |
 |   1  ||  6   |
 \      ||      /
  `------------´
*/

printLeadingZero(Num):-
  (Num>=10, write(Num));
  (write('0'), write(Num)).
