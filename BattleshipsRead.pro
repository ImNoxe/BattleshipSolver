outputFile('./battleships_solved.txt').
inputFile('./battleships_unsolved.txt').


% Defining rules

legalHorizontalCombination('A', '-').
legalHorizontalCombination('-', 'A').
legalHorizontalCombination('-', '*').
legalHorizontalCombination('*', '-').
legalHorizontalCombination('V', '-').
legalHorizontalCombination('-', 'V').
legalHorizontalCombination('-', '<').
legalHorizontalCombination('>', '-').
legalHorizontalCombination('<', '>').
legalHorizontalCombination('<', '+').
legalHorizontalCombination('+', '>').
legalHorizontalCombination('-', '+').
legalHorizontalCombination('+', '-').
legalHorizontalCombination('+', '+').

legalVerticalCombination('-', 'A').
legalVerticalCombination('A', '+').
legalVerticalCombination('A', 'V').
legalVerticalCombination('V', '-').
legalVerticalCombination('+', 'V').
legalVerticalCombination('-', '*').
legalVerticalCombination('*', '-').
legalVerticalCombination('<', '-').
legalVerticalCombination('-', '>').
legalVerticalCombination('-', '<').
legalVerticalCombination('>', '-').
legalVerticalCombination('-', '+').
legalVerticalCombination('+', '-').
legalVerticalCombination('+', '+').


replace(_, _, [], []).
replace(X, Y, [X | T], [Y | NT]) :- replace(X, Y, T, NT).
replace(X, Y, [H | T], [H | NT]) :- H \= X, replace(X, Y, T, NT).


countShips([], Row, 0).

countShips([Element | Tail], Row, Counter) :-
	member(Element, Row),
	countShips(Tail, Row, Counter1),
	Counter is Counter1 + 1.
	
countShips([Element | Tail], Row, Counter) :-
	countShips(Tail, Row, Counter).

fixBoard([], TempBoard, TempBoard).

fixBoard([Head | Tail], TempBoard, FixedBoard) :-
	replace('?', "?", Head, Result),
	append(TempBoard, [Result], ModifiedGrid),
	fixBoard(Tail, ModifiedGrid, FixedBoard).


fillWaterVertical([], [], OldGrid, OldGrid).

fillWaterVertical([VerticalHead | VerticalTail], [GridHead | GridTail], OldGrid, NewGrid) :- 
	VerticalHead == 0,
	replace("?", "-", GridHead, Result),
	append(OldGrid, [Result], ModifiedGrid),
	fillWaterVertical(VerticalTail, GridTail, ModifiedGrid, NewGrid), !.
	
fillWaterVertical([VerticalHead | VerticalTail], [GridHead | GridTail], OldGrid, NewGrid) :-
	countShips(["A", "V", "<", ">", "+", "*", "S"], GridHead, Sum),
	VerticalHead == Sum,
	replace("?", "-", GridHead, Result),
	append(OldGrid, [Result], ModifiedGrid),
	fillWaterVertical(VerticalTail, GridTail, ModifiedGrid, NewGrid).

fillWaterVertical([VerticalHead | VerticalTail], [GridHead | GridTail], OldGrid, NewGrid) :-
	not(VerticalHead == 0),
	append(OldGrid, [GridHead], ModifiedGrid),
	fillWaterVertical(VerticalTail, GridTail, ModifiedGrid, NewGrid).
	


doSolve((battleships(size(Size), boats(Boats), horizontal(Horizontal), vertical(Vertical), grid(Grid))), 
		(battleships(size(Size), boats(Boats), horizontal(Horizontal), vertical(Vertical), grid(NewGrid)))) :-
		fixBoard(Grid, TempBoard, FixedBoard),
		fillWaterVertical(Vertical, FixedBoard, OldGrid, NewGrid),
	
		write('Size: '), write(Size), nl,
		write('Boats: '), write(Boats), nl,
		write('Horizontal: '), write(Horizontal), nl,
		write('Vertical: '), write(Vertical), nl,
		write('Solution: '), write(NewGrid), nl, !.






/********************* writing the result */
writeFullOutput(battleships(size(N),_,_,_,grid(Grid))):- 
  write('size '), write(N), write('x'), write(N), nl, writeGrid(Grid).

writeGrid([]).
writeGrid([E|R]):- writeGridLine(E), writeGrid(R).

writeGridLine([]):- nl.
writeGridLine([E|R]):- (E='?'), !, write(E), write(' '), writeGridLine(R).
writeGridLine([E|R]):- write(E), write(' '), writeGridLine(R).

/********************** reading the input */
readProblem(battleships(size(N),boats(B),horizontal(H),vertical(V),grid(Grid))):- 
  findKW(size), readInt(N), readInt(M), M=N, length(H, N), length(V,N), length(Grid,N), 
  readShips(B), readHorizontal(H), readVertical(V), findKW(hints), readGridLines(N,Grid).

findKW(KW):- string_codes(KW,[H|T]), peek_code(H), readKW([H|T]), !.
findKW(_):- peek_code(-1), !, fail.
findKW(KW):- get_code(_), findKW(KW).

readKW([]):- get_code(_).
readKW([H|T]):- get_code(H), readKW(T).

readShips(L):- findKW(ships), readShipCount(L).

readShipCount(L):- 
  peek_code(C), is_number_code(C,_), readInt(Count), readInt(Size), 
  expandShips(Count,Size,L1), readShipCount(L2), append(L1,L2,L), !.
readShipCount([]).

expandShips(0,_,[]).
expandShips(N,S,[S|T]):- N>0, N1 is N-1, expandShips(N1,S,T).

readHorizontal(L):- findKW(horizontal), readNumberLine(L).
readVertical(L):- findKW(vertical), readNumberLine(L).

readGridLines(_,[]).
readGridLines(N,[H|T]):- length(H,N), readGridLine(H), readGridLines(N,T).

readGridLine([]).
readGridLine([E|T]):- get_code(M), translate(M,E), !, readGridLine(T).

translate(-1,'ERROR: EOF').
translate(63,_).
translate(X,E):- whitespace(X), get_code(Y), translate(Y,E).
translate(X,E):- string_codes(E,[X]).

whitespace(10). whitespace(12). whitespace(32).

readNumberLine([]).
readNumberLine([E|T]):- readInt(E), readNumberLine(T).

readInt(N):- get_code(M), handleCode(M,N).

handleCode(M,N):- is_number_code(M,N1), !, continueInt(N1,N).
handleCode(-1,_):- !, fail. /* EOF */
handleCode(_,N):- readInt(N).

continueInt(O,N):- get_code(M), is_number_code(M,M1), !, H is 10*O+M1, continueInt(H,N).
continueInt(N,N).

is_number_code(N, N1):- N>=48, N<58, N1 is N-48.

/*********************** global control: starting the algorithm and the reading */
run:- inputFile(IF), see(IF), outputFile(F), tell(F), findKW(puzzles), readInt(N),  write('puzzles '), write(N), nl, solveProblems(N), told, seen, !.
run:- told, seen. /* close the files */

solveProblems(0).
solveProblems(N):- N>0, readProblem(P), doSolve(P, S), writeFullOutput(S), !, N1 is N-1, solveProblems(N1).

:- nl,nl,write(' try running "?- run."'), nl,nl,nl.

%:- run.
%:- halt.