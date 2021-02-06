% CSE341 Programming Languages HW4
% Muhammed ÖZKAN 151044084

%PART6
:- use_module(library(clpfd)).


% please editing own your on path
outputpath('C:/Users/muham/Desktop/pl/2020/Hw4/output.txt').




%rules


loop_through_list(File, List) :-
    member(Element, List),
    write(File, Element),
    write(File, ' '),
    fail.

write_list_to_file(Filename,List) :-
    open(Filename, write, File),
    \+ loop_through_list(File, List),
    close(File).



puzzle(RowProp, ColProp, Table) :-    makerows(RowProp, Table),    transpose(Table, TableT),    makerows(ColProp, TableT).

makerows([], []).

makerows([Col|Cols], [Row|Rows]) :-    makerow(Col, Row),    makerows(Cols, Rows).

makerow(Ks, Row) :-    sum(Ks,  #=, Ones),    sum(Row, #=, Ones),    arcs(Ks, Arcs, start, End),    append(Row, [0], RowZ),
    automaton(RowZ, [source(start), sink(End)], [arc(start,0,start) | Arcs]).


arcs([], [], End, End).

arcs([K|Ks], Arcs, CurrentStatement, End) :-    gensym(statement, NextStatement),
    (K == 0 ->        Arcs = [arc(CurrentStatement,0,CurrentStatement), arc(CurrentStatement,0,NextStatement) | Rest],        arcs(Ks, Rest, NextStatement, End);
        Arcs = [arc(CurrentStatement,1,NextStatement) | Rest],        K1 #= K-1,        arcs([K1|Ks], Rest, NextStatement, End)    ).


make_table(Table, X, Y, Vars) :-    length(Table,X),    make_rows(Table, Y, Vars).

make_rows([], _, []).

make_rows([Row|Rows], Len, Vars) :-    length(Row, Len),    make_rows(Rows, Len, Vars0),    append(Row, Vars0, Vars).

print([]).

print([Row|Rows]) :-    print_row(Row),outputpath(Outpath),open(Outpath, append, Handle), write(Handle, '\n'), close(Handle) ,    print(Rows).

print_row([]) :- nl.

print_row([X|Row]) :-    (X == 0 ->        write(' '),outputpath(Outpath),open(Outpath, append, Handle), write(Handle, ' '), close(Handle)    ;
        write('x'),outputpath(Outpath),open(Outpath, append, Handle), write(Handle, 'x'), close(Handle)     ),    print_row(Row).
	
	
	

	
example1([[3], [2,1], [3,2], [2,2], [6], [1,5], [6], [1], [2]],
 [[1,2], [3,1], [1,5], [7,1], [5], [3], [4], [3]]).
	
example2( [[3,1], [2,4,1], [1,3,3], [2,4], [3,3,1,3], [3,2,2,1,3], [2,2,2,2,2],
[2,1,1,2,1,1], [1,2,1,4], [1,1,2,2], [2,2,8], [2,2,2,4], [1,2,2,1,1,1],
[3,3,5,1], [1,1,3,1,1,2], [2,3,1,3,3], [1,3,2,8], [4,3,8], [1,4,2,5], [1,4,2,2],
[4,2,5], [5,3,5], [4,1,1], [4,2], [3,3]],
[[2,3], [3,1,3], [3,2,1,2], [2,4,4], [3,4,2,4,5], [2,5,2,4,6], [1,4,3,4,6,1],
[4,3,3,6,2], [4,2,3,6,3], [1,2,4,2,1], [2,2,6], [1,1,6], [2,1,4,2], [4,2,6],
[1,1,1,1,4], [2,4,7], [3,5,6], [3,2,4,2], [2,2,2], [6,3]]).	

example3( [[5], [2,3,2], [2,5,1], [2,8], [2,5,11], [1,1,2,1,6], [1,2,1,3],[2,1,1],
[2,6,2], [15,4], [10,8], [2,1,4,3,6], [17], [17], [18], [1,14], [1,1,14], [5,9],
[8], [7]], [[5], [3,2], [2,1,2], [1,1,1], [1,1,1], [1,3], [2,2], [1,3,3],
[1,3,3,1], [1,7,2], [1,9,1], [1,10], [1,10], [1,3,5], [1,8],
[2,1,6],[3,1,7], [4,1,7], [6,1,8], [6,10], [7,10], [1,4,11], [1,2,11], [2,12],
[3,13]] ).



test1 :-
    example1(Rows, Cols),length(Rows, VSize),length(Cols, HSize),
    make_table(Table, VSize, HSize, Vars),
    puzzle(Rows, Cols, Table),    label(Vars),    print(Table).
	
test2 :-
    example2(Rows, Cols),length(Rows, VSize),length(Cols, HSize),
    make_table(Table, VSize, HSize, Vars),
    puzzle(Rows, Cols, Table),    label(Vars),    print(Table).
	
test3 :-
    example3(Rows, Cols),length(Rows, VSize),length(Cols, HSize),
    make_table(Table, VSize, HSize, Vars),
    puzzle(Rows, Cols, Table),    label(Vars),    print(Table).
	
test:-outputpath(Outpath),open(Outpath, write, Handle), write(Handle, ''), close(Handle),test1,write('\n\n'),test2,write('\n\n'),test3.

solve(Rows, Cols):-
length(Rows, VSize),length(Cols, HSize),
    make_table(Table, VSize, HSize, Vars),
    puzzle(Rows, Cols, Table),    label(Vars) , outputpath(Outpath),open(Outpath, write, Handle), write(Handle, ''), close(Handle),  print(Table).