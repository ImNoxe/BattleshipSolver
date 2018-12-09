outputFile('./battleships_solved.txt').
inputFile('./battleships_unsolved.txt').


% Defining rules
waterPlacementRule("*", "-", "-", "-").
waterPlacementRule("-", "*", "-", "-").
waterPlacementRule("-", "-", "*", "-").
waterPlacementRule("-", "-", "-", "*").

waterPlacementRule("A", "-", "S", "-").
waterPlacementRule("-", "A", "-", "S").
waterPlacementRule("-", "-", "A", "-").
waterPlacementRule("-", "-", "-", "A").

waterPlacementRule("V", "-", "-", "-").
waterPlacementRule("-", "V", "-", "-").
waterPlacementRule("S", "-", "V", "-").
waterPlacementRule("-", "S", "-", "V").

waterPlacementRule("<", "S", "-", "-").
waterPlacementRule("-", "<", "-", "-").
waterPlacementRule("-", "-", "<", "S").
waterPlacementRule("-", "-", "-", "<").

waterPlacementRule(">", "-", "-", "-").
waterPlacementRule("S", ">", "-", "-").
waterPlacementRule("-", "-", ">", "-").
waterPlacementRule("-", "-", "S", ">").


% Replace All Equal Characters in List.
replaceList(_, _, [], []).
replaceList(X, Y, [X | T], [Y | NT]) :- replaceList(X, Y, T, NT).
replaceList(X, Y, [H | T], [H | NT]) :- H \= X, replaceList(X, Y, T, NT).

% Replace Single Character by Index in List.
replaceByIndex([_|T], 0, X, [X|T]).
replaceByIndex([H|T], I, X, [H|R]):- I > -1, NI is I-1, replaceByIndex(T, NI, X, R), !.
replaceByIndex(L, _, _, L).

% Get by Index
getByIndex([X], 0, X).
getByIndex([H|_], 0, H).
getByIndex([_|T], I, E):- 
	NewIndex is I-1, getByIndex(T, NewIndex, E).
	
% Rotate List.
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).
		

% List equals list.
listEquality([], []).
listEquality([H1|R1], [H2|R2]):-
    H1 = H2,
    listEquality(R1, R2).

% Count Number of Ship Pieces in Line.
count(_,  [], 0).
count(X, [X|T], N):- !, 
	count(X,T,N1),
	N is N1 + 1.
	
count(X, [_|T],N):-
	count(X,T,N).

countElements([], _, Temp, Temp).
countElements([Element | Tail], Row, Temp, N):-
	count(Element, Row, NewTemp),
	NewestTemp is Temp + NewTemp,
	countElements(Tail, Row, NewestTemp, N).


% Replace 'Questionmarks' with "Questionmarks".
fixGrid([], TempBoard, TempBoard).
fixGrid([Head | Tail], TempBoard, FixedBoard) :-
	replaceList('?', "?", Head, Result),
	append(TempBoard, [Result], ModifiedGrid),
	fixGrid(Tail, ModifiedGrid, FixedBoard).

% Fill Water Around Boat Pieces
fillWaterAroundBoat([], NewGrid, NewGrid, []).
fillWaterAroundBoat([_], NewGrid, Result, FinalList) :-
	append(NewGrid, [FinalList], TempGrid),
	fillWaterAroundBoat([], TempGrid, Result, []).
	
fillWaterAroundBoat([First, Second | Tail], NewGrid, Result, _) :-
	getListOfFour(First, Second, _, _, FinishedFirstList, FinishedSecondList),
	append([FinishedSecondList], Tail, NewTail),
	append(NewGrid, [FinishedFirstList], TempGrid),
	fillWaterAroundBoat(NewTail, TempGrid, Result, FinishedSecondList).
	

getListOfFour([], [], FirstList, SecondList, FirstList, SecondList).
getListOfFour([First], [First2], FirstListTemp, SecondListTemp, FirstList, SecondList):-
	append(FirstListTemp, [First], NewFirstList),
	append(SecondListTemp, [First2], NewSecondList),
	getListOfFour([],[], NewFirstList, NewSecondList, FirstList, SecondList).
	
getListOfFour([First, Second | Tail1], [First2, Second2 | Tail2], FirstList, SecondList, FinishedFirstList, FinishedSecondList) :-
	append([First, Second], [First2, Second2], ListOfFour),
	fillWaterByRule(ListOfFour, _, Result),
	appendListOfFourToNewGrid(Result, FirstList, SecondList, FirstList2, SecondList2),
	getByIndex(Result, 1, E1),
	append([E1], Tail1, NewTail1),
	getByIndex(Result, 3, E2),
	append([E2], Tail2, NewTail2),
	getListOfFour(NewTail1, NewTail2, FirstList2, SecondList2, FinishedFirstList, FinishedSecondList).
	
	
appendListOfFourToNewGrid([], FirstList, SecondList, FirstList, SecondList).
appendListOfFourToNewGrid([First, _, Third, _], [], [], FirstList2, SecondList2) :-
	appendListOfFourToNewGrid([], [First], [Third], FirstList2, SecondList2).

appendListOfFourToNewGrid([First, _, Third, _], FirstList, SecondList, FirstList2, SecondList2) :-
	append(FirstList, [First], NewFirstList),
	append(SecondList, [Third], NewSecondList),
	appendListOfFourToNewGrid([], NewFirstList, NewSecondList, FirstList2, SecondList2).


fillWaterByRule([], Result, Result).
fillWaterByRule([First, Second, Third, Fourth], _, Result):-
	member(First, ["A", "V", "<", ">", "*"]),
	waterPlacementRule(First, X, Y, Z),
	replaceByIndex([First, Second, Third, Fourth], 1, X, L),
	replaceByIndex(L, 2, Y, L2),
	replaceByIndex(L2, 3, Z, L3),
	fillWaterByRule([], L3, Result).

fillWaterByRule([First, Second, Third, Fourth], _, Result):-
	member(Second, ["A", "V", "<", ">", "*"]),
	waterPlacementRule(X, Second, Y, Z),
	replaceByIndex([First, Second, Third, Fourth], 0, X, L),
	replaceByIndex(L, 2, Y, L2),
	replaceByIndex(L2, 3, Z, L3),
	fillWaterByRule([], L3, Result).
	
fillWaterByRule([First, Second, Third, Fourth], _, Result):-
	member(Third, ["A", "V", "<", ">", "*"]),
	waterPlacementRule(X, Y, Third, Z),
	replaceByIndex([First, Second, Third, Fourth], 0, X, L),
	replaceByIndex(L, 1, Y, L2),
	replaceByIndex(L2, 3, Z, L3),
	fillWaterByRule([], L3, Result).
	
fillWaterByRule([First, Second, Third, Fourth], _, Result):-
	member(Fourth, ["A", "V", "<", ">", "*"]),
	waterPlacementRule(X, Y, Z, Fourth),
	replaceByIndex([First, Second, Third, Fourth], 0, X, L),
	replaceByIndex(L, 1, Y, L2),
	replaceByIndex(L2, 2, Z, L3),
	fillWaterByRule([], L3, Result).
	
fillWaterByRule([First, Second, Third, Fourth], _, Result):-
	member(First, ["S", "+"]),
	replaceByIndex([First, Second, Third, Fourth], 3, "-", NewList),
	fillWaterByRule([], NewList, Result).
	
fillWaterByRule([First, Second, Third, Fourth], _, Result):-
	member(Second, ["S", "+"]),
	replaceByIndex([First, Second, Third, Fourth], 2, "-", NewList),
	fillWaterByRule([], NewList, Result).
	
fillWaterByRule([First, Second, Third, Fourth], _, Result):-
	member(Third, ["S", "+"]),
	replaceByIndex([First, Second, Third, Fourth], 1, "-", NewList),
	fillWaterByRule([], NewList, Result).
	
fillWaterByRule([First, Second, Third, Fourth], _, Result):-
	member(Fourth, ["S", "+"]),
	replaceByIndex([First, Second, Third, Fourth], 0, "-", NewList),
	fillWaterByRule([], NewList, Result).
	
fillWaterByRule([First, Second, Third, Fourth], _, Result):-
	fillWaterByRule([], [First, Second, Third, Fourth], Result).
	

% Fill Water in Lines.
fillWater([], [], OldGrid, NewGrid) :-
	transpose(OldGrid, NewGrid).

fillWater([Head | Tail], [GridHead | GridTail], OldGrid, NewGrid) :- 
	Head == 0,
	replaceList("?", "-", GridHead, Result),
	append(OldGrid, [Result], ModifiedGrid),
	fillWater(Tail, GridTail, ModifiedGrid, NewGrid), !.
	
fillWater([Head | Tail], [GridHead | GridTail], OldGrid, NewGrid) :-
	countElements(["A", "V", "<", ">", "+", "*", "S"], GridHead, 0, Sum),
	Head == Sum,
	replaceList("?", "-", GridHead, Result),
	append(OldGrid, [Result], ModifiedGrid),
	fillWater(Tail, GridTail, ModifiedGrid, NewGrid).

fillWater([Head | Tail], [GridHead | GridTail], OldGrid, NewGrid) :-
	not(Head == 0),
	append(OldGrid, [GridHead], ModifiedGrid),
	fillWater(Tail, GridTail, ModifiedGrid, NewGrid).
	
fillShip([], [], OldGrid, NewGrid) :-
	transpose(OldGrid, NewGrid).

fillShip([Head | Tail], [GridHead | GridTail], OldGrid, NewGrid) :-
	countElements(["A", "V", "<", ">", "+", "*", "S", "?"], GridHead, 0, Sum),
	Head == Sum,
	replaceList("?", "S", GridHead, Result),
	append(OldGrid, [Result], ModifiedGrid),
	fillShip(Tail, GridTail, ModifiedGrid, NewGrid).
	
fillShip([Head | Tail], [GridHead | GridTail], OldGrid, NewGrid) :-
	not(Head == 0),
	append(OldGrid, [GridHead], ModifiedGrid),
	fillShip(Tail, GridTail, ModifiedGrid, NewGrid).
	

replaceShipPartToS([], TempBoard, TempBoard).
replaceShipPartToS([Head | Tail], TempBoard, FixedBoard):-
	replaceList("<", "S", Head, L1),
	replaceList(">", "S", L1, L2),
	replaceList("A", "S", L2, L3),
	replaceList("V", "S", L3, L4),
	replaceList("+", "S", L4, L5),
	replaceList("*", "S", L5, L6),
	append(TempBoard, [L6], ModifiedGrid),
	replaceShipPartToS(Tail, ModifiedGrid, FixedBoard).
	
	
isBoardSolved([]).
isBoardSolved([Head | Tail]) :-
	not(member("?", Head)),
	isBoardSolved(Tail).
	
	
solveBoard(_, [], _, _, Grid, Grid).
solveBoard(Size, Boats, Horizontal, Vertical, Grid, FinishedBoard) :-
	not(isBoardSolved(Grid)), !,
	fillWater(Vertical, Grid, _, VerticallyFilledGrid),
	fillWater(Horizontal, VerticallyFilledGrid, _, HorizontallyFilledGrid),
	fillShip(Vertical, HorizontallyFilledGrid, _, VerticallyFilledGrid2),
	fillShip(Horizontal, VerticallyFilledGrid2, _, HorizontallyFilledGrid2),
	fillWaterAroundBoat(HorizontallyFilledGrid2, [], Return2, []),
	solveBoard(Size, Boats, Horizontal, Vertical, Return2, FinishedBoard).

solveBoard(Size, _, Horizontal, Vertical, Grid, FinishedBoard) :-
	isBoardSolved(Grid), !,
	solveBoard(Size, [], Horizontal, Vertical, Grid, FinishedBoard).
	
% Solve Board
doSolve((battleships(size(Size), boats(Boats), horizontal(Horizontal), vertical(Vertical), grid(Grid))), 
		(battleships(size(Size), boats(Boats), horizontal(Horizontal), vertical(Vertical), grid(FinishGrid)))) :-
		fixGrid(Grid, _, FixedGrid),
		solveBoard(Size, Boats, Horizontal, Vertical, FixedGrid, FinishedBoard),
		replaceShipPartToS(FinishedBoard, [], FinishGrid), !.


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