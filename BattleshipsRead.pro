outputFile('./battleships_solved.txt').
inputFile('./battleships_unsolved.txt').


% Defining rules
/*waterPlacementRule("-", "*").
waterPlacementRule("*", "-").

waterPlacementRule("-", "A").
waterPlacementRule("A", "S").
waterPlacementRule("A", "+").
waterPlacementRule("+", "V").
waterPlacementRule("S", "V").
waterPlacementRule("V", "-").
waterPlacementRule("A", "V").


waterPlacementRule("-", "<").
waterPlacementRule("<", "S").
waterPlacementRule("<", "+").
waterPlacementRule("+", ">").
waterPlacementRule("S", ">").
waterPlacementRule(">", "-").
waterPlacementRule("<", ">").*/


% Replace All Equal Characters in List.
replaceList(_, _, [], []).
replaceList(X, Y, [X | T], [Y | NT]) :- replaceList(X, Y, T, NT).
replaceList(X, Y, [H | T], [H | NT]) :- H \= X, replaceList(X, Y, T, NT).

% Replace Single Character by Index in List.
replaceByIndex([_|T], 0, X, [X|T]).
replaceByIndex([H|T], I, X, [H|R]):- I > -1, NI is I-1, replaceByIndex(T, NI, X, R), !.
replaceByIndex(L, _, _, L).

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
countShips([], _, 0).
countShips([Element | Tail], Row, Counter) :-
	member(Element, Row),
	countShips(Tail, Row, Counter1),
	Counter is Counter1 + 1.

countShips([_ | Tail], Row, Counter) :-
	countShips(Tail, Row, Counter).

% Replace 'Questionmarks' with "Questionmarks".
fixGrid([], TempBoard, TempBoard).
fixGrid([Head | Tail], TempBoard, FixedBoard) :-
	replaceList('?', "?", Head, Result),
	append(TempBoard, [Result], ModifiedGrid),
	fixGrid(Tail, ModifiedGrid, FixedBoard).
	
	
	
fillWaterAroundBoat([], NewGrid, NewGrid, []).
	%write(Result).
	
fillWaterAroundBoat([First], NewGrid, Result, FinalList) :-
	append(NewGrid, [FinalList], TempGrid),
	fillWaterAroundBoat([], TempGrid, Result, []).

	
fillWaterAroundBoat([First, Second | Tail], NewGrid, Result, FinalList) :-
	getListOfFour(First, Second, FirstList, SecondList, FinishedFirstList, FinishedSecondList),
	append([Second], Tail, NewTail),
	append(NewGrid, [FinishedFirstList], TempGrid),
	fillWaterAroundBoat(NewTail, TempGrid, Result, FinishedSecondList).
	


getListOfFour([], [], FirstList, SecondList, FirstList, SecondList).

getListOfFour([First], [First2], FirstList, SecondList, FirstList, SecondList).

getListOfFour([First, Second | Tail], [First2, Second2 | Tail2], FirstList, SecondList, FinishedFirstList, FinishedSecondList) :-
	append([First, Second], [First2, Second2], ListOfFour),
	
	%Checks here
	
	appendListOfFourToNewGrid(ListOfFour, FirstList, SecondList, FirstList2, SecondList2),
	
	append([Second], Tail, NewTail),
	append([Second2], Tail2, NewTail2),
	getListOfFour(NewTail, NewTail2, FirstList2, SecondList2, FinishedFirstList, FinishedSecondList).

appendListOfFourToNewGrid([], FirstList, SecondList, FirstList, SecondList).
	
appendListOfFourToNewGrid([First, Second, Third, Fourth], [], [], FirstList2, SecondList2) :-
	append([First], [Second], UpperList),
	append([Third], [Fourth], BottomList),
	appendListOfFourToNewGrid([], UpperList, BottomList, FirstList2, SecondList2).

appendListOfFourToNewGrid([First, Second, Third, Fourth], FirstList, SecondList, FirstList2, SecondList2) :-
	append(FirstList, [Second], NewFirstList),
	append(SecondList, [Fourth], NewSecondList),
	appendListOfFourToNewGrid([], NewFirstList, NewSecondList, FirstList2, SecondList2).


% Fill Water in Lines.
fillWater([], [], OldGrid, NewGrid) :-
	transpose(OldGrid, NewGrid).

fillWater([Head | Tail], [GridHead | GridTail], OldGrid, NewGrid) :- 
	Head == 0,
	replaceList("?", "-", GridHead, Result),
	append(OldGrid, [Result], ModifiedGrid),
	fillWater(Tail, GridTail, ModifiedGrid, NewGrid), !.
	
fillWater([Head | Tail], [GridHead | GridTail], OldGrid, NewGrid) :-
	countShips(["A", "V", "<", ">", "+", "*", "S"], GridHead, Sum),
	Head == Sum,
	replaceList("?", "-", GridHead, Result),
	append(OldGrid, [Result], ModifiedGrid),
	fillWater(Tail, GridTail, ModifiedGrid, NewGrid).

fillWater([Head | Tail], [GridHead | GridTail], OldGrid, NewGrid) :-
	not(Head == 0),
	append(OldGrid, [GridHead], ModifiedGrid),
	fillWater(Tail, GridTail, ModifiedGrid, NewGrid).
	


% Solve Board
doSolve((battleships(size(Size), boats(Boats), horizontal(Horizontal), vertical(Vertical), grid(Grid))), 
		(battleships(size(Size), boats(Boats), horizontal(Horizontal), vertical(Vertical), grid(Return)))) :-
		fixGrid(Grid, _, FixedGrid),
		fillWater(Vertical, FixedGrid, _, VerticallyFilledGrid),
		fillWater(Horizontal, VerticallyFilledGrid, _, HorizontallyFilledGrid),
		fillWaterAroundBoat(HorizontallyFilledGrid, [], Return, []),
	
		write('Size: '), write(Size), nl,
		write('Boats: '), write(Boats), nl,
		write('Horizontal: '), write(Horizontal), nl,
		write('Vertical: '), write(Vertical), nl,
		write('Solution: '), write(Return), nl, !.






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