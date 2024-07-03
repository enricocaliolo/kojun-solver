% Importa a biblioteca CLP(FD) para restrições sobre inteiros
:- use_module(library(clpfd)).

% Importa o módulo games, que deve conter definições como board_6x6, regions_6x6, etc.
:- use_module(games).

% Função para imprimir o tabuleiro
% Caso base: se a lista estiver vazia, não faz nada
print_board([]).
% Caso recursivo: imprime a primeira linha e chama a função para o restante do tabuleiro
print_board([Row|Rest]) :-
    writeln(Row),
    print_board(Rest).

% Função para encontrar uma célula vazia no tabuleiro
find_empty_cell(Board, (I, J)) :-
    % Encontra a linha I no tabuleiro
    nth0(I, Board, Row),
    % Encontra a coluna J na linha, onde o valor é 0
    nth0(J, Row, 0).

% Função para encontrar a região de uma célula vazia
find_region((I, J), Regions, Region, RegionCells) :-
    % Encontra a região à qual a célula (I, J) pertence
    member((Region, (I, J)), Regions),
    % Encontra todas as células pertencentes à mesma região
    findall((X, Y), member((Region, (X, Y)), Regions), RegionCells).

% Função para verificar se um número é válido em uma célula vazia
is_valid(Board, Regions, (I, J), Num) :-
    % Encontra a região da célula (I, J)
    find_region((I, J), Regions, Region, RegionCells),
    % Verifica se o número não está presente na região
    \+ (member((X, Y), RegionCells), nth0(X, Board, Row), nth0(Y, Row, Num)),
    % Verifica o tamanho máximo permitido na região
    length(RegionCells, MaxNum),
    % Verifica se o número é menor ou igual ao máximo permitido
    Num =< MaxNum,
    % Verifica restrições adjacentes
    adjacent_constraints(Board, (I, J), Num, Region, Regions).

% Função para verificar as restrições adjacentes
adjacent_constraints(Board, (I, J), Num, Region, Regions) :-
    % Verifica a célula acima
    check_above(Board, Regions, (I, J), Num, Region),
    % Verifica a célula abaixo
    check_below(Board, Regions, (I, J), Num, Region),
    % Verifica a célula à esquerda
    check_left(Board, (I, J), Num),
    % Verifica a célula à direita
    check_right(Board, (I, J), Num).

% Checa se o número acima é válido
check_above(Board, Regions, (I, J), Num, Region) :-
    % Calcula o índice da linha acima
    I1 is I - 1,
    % Verifica se o índice está dentro dos limites do tabuleiro
    (within_bounds(Board, I1, J) ->
        nth0(I1, Board, Row),
        nth0(J, Row, AboveNum),
        % Se a célula acima estiver vazia, é válido
        (AboveNum == 0 -> true;
            % Caso contrário, verifica a região e o valor da célula acima
            (find_region((I1, J), Regions, AboveRegion, _),
            (AboveRegion \= Region; AboveNum > Num),
            AboveNum \= Num))
    ; true).

% Checa se o número abaixo é válido
check_below(Board, Regions, (I, J), Num, Region) :-
    % Calcula o índice da linha abaixo
    I1 is I + 1,
    % Verifica se o índice está dentro dos limites do tabuleiro
    (within_bounds(Board, I1, J) ->
        nth0(I1, Board, Row),
        nth0(J, Row, BelowNum),
        % Se a célula abaixo estiver vazia, é válido
        (BelowNum == 0 -> true;
            % Caso contrário, verifica a região e o valor da célula abaixo
            (find_region((I1, J), Regions, BelowRegion, _),
            (BelowRegion \= Region; BelowNum < Num),
            BelowNum \= Num))
    ; true).

% Checa se o número à esquerda é válido
check_left(Board, (I, J), Num) :-
    % Calcula o índice da coluna à esquerda
    J1 is J - 1,
    % Verifica se o índice está dentro dos limites do tabuleiro
    (within_bounds(Board, I, J1) ->
        nth0(I, Board, Row),
        nth0(J1, Row, LeftNum),
        % Verifica se o valor da célula à esquerda é diferente de Num
        LeftNum \= Num
    ; true).

% Checa se o número à direita é válido
check_right(Board, (I, J), Num) :-
    % Calcula o índice da coluna à direita
    J1 is J + 1,
    % Verifica se o índice está dentro dos limites do tabuleiro
    (within_bounds(Board, I, J1) ->
        nth0(I, Board, Row),
        nth0(J1, Row, RightNum),
        % Verifica se o valor da célula à direita é diferente de Num
        RightNum \= Num
    ; true).

% Função para verificar se os índices estão dentro dos limites do tabuleiro
within_bounds(Board, I, J) :-
    % Obtém o número de linhas do tabuleiro
    length(Board, N),
    % Verifica se o índice da linha está dentro dos limites
    I >= 0, I < N,
    % Obtém a linha correspondente
    nth0(I, Board, Row),
    % Obtém o número de colunas da linha
    length(Row, M),
    % Verifica se o índice da coluna está dentro dos limites
    J >= 0, J < M.

% Função para resolver o tabuleiro
solve(Board, Regions, FinalBoard) :-
    % Encontra uma célula vazia
    ( find_empty_cell(Board, EmptyCell) ->
        % Resolve a célula vazia
        solve_cell(Board, Regions, EmptyCell, FinalBoard)
    ; % Se não houver células vazias, o tabuleiro está completo
      FinalBoard = Board
    ).

% Função para resolver uma célula vazia
solve_cell(Board, Regions, EmptyCell, FinalBoard) :-
    % Obtém o tamanho do tabuleiro
    length(Board, N),
    % Tenta números de 1 até N
    between(1, N, Num),
    % Verifica se o número é válido na célula vazia
    is_valid(Board, Regions, EmptyCell, Num),
    % Substitui a célula vazia pelo número
    replace(Board, EmptyCell, Num, NewBoard),
    % Resolve o restante do tabuleiro
    solve(NewBoard, Regions, FinalBoard).

% Função auxiliar para substituir o valor em uma célula
replace(Board, (I, J), Val, NewBoard) :-
    % Obtém a linha correspondente
    nth0(I, Board, Row, RestRows),
    % Substitui o valor na coluna J da linha
    nth0(J, Row, _, RestElems),
    nth0(J, NewRow, Val, RestElems),
    % Substitui a linha no tabuleiro
    nth0(I, NewBoard, NewRow, RestRows).

% Função principal para o tabuleiro 17x17
main_17x17 :-
    % Obtém o tabuleiro e as regiões
    board_17x17(Board),
    regions_17x17(Regions),
    % Achata o tabuleiro em uma lista
    flatten(Board, FlatBoard),
    % Define o domínio das variáveis
    FlatBoard ins 0..17,
    % Resolve o tabuleiro e aplica heurísticas de labeling
    ( solve(Board, Regions, FinalBoard)
    -> labeling([ff, bisect], FlatBoard),
       % Imprime o tabuleiro final
       print_board(FinalBoard)
    ; % Se não houver solução, imprime uma mensagem
      writeln('No solution found')
    ).

% Função principal para o tabuleiro 10x10
main_10x10 :-
    % Obtém o tabuleiro e as regiões
    board_10x10(Board),
    regions_10x10(Regions),
    % Achata o tabuleiro em uma lista
    flatten(Board, FlatBoard),
    % Define o domínio das variáveis
    FlatBoard ins 0..10,
    % Resolve o tabuleiro e aplica heurísticas de labeling
    ( solve(Board, Regions, FinalBoard)
    -> labeling([ff], FlatBoard),
       % Imprime o tabuleiro final
       print_board(FinalBoard)
    ; % Se não houver solução, imprime uma mensagem
      writeln('No solution found')
    ).

% Função principal para o tabuleiro 6x6
main_6x6 :-
    % Obtém o tabuleiro e as regiões
    board_6x6(Board),
    regions_6x6(Regions),
    % Achata o tabuleiro em uma lista
    flatten(Board, FlatBoard),
    % Define o domínio das variáveis
    FlatBoard ins 0..6,
    % Resolve o tabuleiro e aplica heurísticas de labeling
    ( solve(Board, Regions, FinalBoard)
    -> labeling([ff], FlatBoard),
       % Imprime o tabuleiro final
       print_board(FinalBoard)
    ; % Se não houver solução, imprime uma mensagem
      writeln('No solution found')
    ).
