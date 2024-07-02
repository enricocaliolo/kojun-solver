:- use_module(library(clpfd)).
:- use_module(games).  % Certifique-se que games contém as definições board_6x6, regions_6x6, etc.

% Função para imprimir o tabuleiro
print_board([]).
print_board([Row|Rest]) :-
    writeln(Row),
    print_board(Rest).

% Função para encontrar uma célula vazia
find_empty_cell(Board, (I, J)) :-
    nth0(I, Board, Row),
    nth0(J, Row, 0),
    format('Found empty cell at: (~w, ~w)~n', [I, J]).

% Função para encontrar a região de uma célula vazia
find_region((I, J), Regions, Region, RegionCells) :-
    member((Region, (I, J)), Regions),
    findall((X, Y), member((Region, (X, Y)), Regions), RegionCells).

% Função para verificar se um número é válido em uma célula vazia
is_valid(Board, Regions, (I, J), Num) :-
    find_region((I, J), Regions, Region, RegionCells),
    \+ (member((X, Y), RegionCells), nth0(X, Board, Row), nth0(Y, Row, Num)),
    length(RegionCells, MaxNum),
    Num =< MaxNum,
    adjacent_constraints(Board, (I, J), Num, Region, Regions).

% Função para verificar as restrições adjacentes
adjacent_constraints(Board, (I, J), Num, Region, Regions) :-
    check_above(Board, Regions, (I, J), Num, Region),
    check_below(Board, Regions, (I, J), Num, Region),
    check_left(Board, (I, J), Num),
    check_right(Board, (I, J), Num).

% Checa se o número em cima é válido
check_above(Board, Regions, (I, J), Num, Region) :-
    I1 is I - 1,
    (within_bounds(Board, I1, J) ->
        nth0(I1, Board, Row),
        nth0(J, Row, AboveNum),
        (AboveNum == 0 -> true;
            (find_region((I1, J), Regions, AboveRegion, _),
            (AboveRegion \= Region; AboveNum > Num),
            AboveNum \= Num))
    ; true).

% Checa se o número embaixo é válido
check_below(Board, Regions, (I, J), Num, Region) :-
    I1 is I + 1,
    (within_bounds(Board, I1, J) ->
        nth0(I1, Board, Row),
        nth0(J, Row, BelowNum),
        (BelowNum == 0 -> true;
            (find_region((I1, J), Regions, BelowRegion, _),
            (BelowRegion \= Region; BelowNum < Num),
            BelowNum \= Num))
    ; true).

% Checa se o número à esquerda é válido
check_left(Board, (I, J), Num) :-
    J1 is J - 1,
    (within_bounds(Board, I, J1) ->
        nth0(I, Board, Row),
        nth0(J1, Row, LeftNum),
        LeftNum \= Num
    ; true).

% Checa se o número à direita é válido
check_right(Board, (I, J), Num) :-
    J1 is J + 1,
    (within_bounds(Board, I, J1) ->
        nth0(I, Board, Row),
        nth0(J1, Row, RightNum),
        RightNum \= Num
    ; true).

% Função para verificar se os índices estão dentro dos limites do tabuleiro
within_bounds(Board, I, J) :-
    length(Board, N),
    I >= 0, I < N,
    nth0(I, Board, Row),
    length(Row, M),
    J >= 0, J < M.

% Função para resolver o tabuleiro
solve(Board, Regions, FinalBoard) :-
    ( find_empty_cell(Board, EmptyCell) ->
        solve_cell(Board, Regions, EmptyCell, FinalBoard)
    ; FinalBoard = Board
    ).

solve_cell(Board, Regions, EmptyCell, FinalBoard) :-
    length(Board, N),
    between(1, N, Num),
    is_valid(Board, Regions, EmptyCell, Num),
    replace(Board, EmptyCell, Num, NewBoard),
    solve(NewBoard, Regions, FinalBoard).

% Função auxiliar para substituir o valor em uma célula
replace(Board, (I, J), Val, NewBoard) :-
    nth0(I, Board, Row, RestRows),
    nth0(J, Row, _, RestElems),
    nth0(J, NewRow, Val, RestElems),
    nth0(I, NewBoard, NewRow, RestRows).

% Funções principais
main_17x17 :-
    board_17x17(Board),
    regions_17x17(Regions),
    flatten(Board, FlatBoard),
    FlatBoard ins 0..17,  % Definindo o domínio para variáveis 17x17
    ( solve(Board, Regions, FinalBoard)
    -> labeling([ff, bisect], FlatBoard),  % Usando heurísticas de labeling
       print_board(FinalBoard)
    ; writeln('No solution found')
    ).

main_10x10 :-
    board_10x10(Board),
    regions_10x10(Regions),
    flatten(Board, FlatBoard),
    FlatBoard ins 0..10,  % Definindo o domínio para variáveis 10x10
    ( solve(Board, Regions, FinalBoard)
    -> labeling([ff], FlatBoard),
       print_board(FinalBoard)
    ; writeln('No solution found')
    ).

main_6x6 :-
    board_6x6(Board),
    regions_6x6(Regions),
    flatten(Board, FlatBoard),
    FlatBoard ins 0..6,  % Definindo o domínio para variáveis 6x6
    ( solve(Board, Regions, FinalBoard)
    -> labeling([ff], FlatBoard),
       print_board(FinalBoard)
    ; writeln('No solution found')
    ).
