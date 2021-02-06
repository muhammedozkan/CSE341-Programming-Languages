% CSE341 Programming Languages HW4
% Muhammed ÖZKAN 151044084

%PART5

%rules

% please editing own your on path
inputpath('C:/Users/muham/Desktop/pl/2020/Hw4/input.txt').
outputpath('C:/Users/muham/Desktop/pl/2020/Hw4/output.txt').



my_read_file(File,List):-
    open(File, read, Stream),
    read_line(Stream, List),
    close(Stream).

read_line(Stream, List) :-
    read_line_to_codes(Stream, Line),
	Line = [_|L1],
	remove_last(L1, L3),
    atom_codes(A, L3),
    atomic_list_concat(As, ',', A),
    maplist(atom_number, As, List).



remove_last(L, LR) :-
    append(LR, [_], L).



loop_through_list(File, List) :-
    member(Element, List),
    write(File, Element),
    write(File, ' '),
    fail.

write_list_to_file(Filename,List) :-
    open(Filename, write, File),
    \+ loop_through_list(File, List),
    close(File).



equation(List) :- chunk(List,LeftList,RightList),   convert(RightList,RightTerm),convert(LeftList,LeftTerm),  LeftTerm =:= RightTerm,writef('%w = %w\n',[LeftTerm,RightTerm]),
outputpath(Outpath),write_list_to_file(Outpath,[LeftTerm,=,RightTerm]) ,  fail. equation(_).


convert([X],X). 

convert(List,Term) :- chunk(List,LeftList,RightList),  convert(RightList,RightTerm),convert(LeftList,LeftTerm), calculation(LeftTerm,RightTerm,Term).

calculation(LeftTerm,RightTerm,LeftTerm*RightTerm).
calculation(LeftTerm,RightTerm,LeftTerm/RightTerm) :- RightTerm =\= 0.
calculation(LeftTerm,RightTerm,LeftTerm+RightTerm).
calculation(LeftTerm,RightTerm,LeftTerm-RightTerm).
 


chunk(List,Term1,Term2) :- append(Term1,Term2,List), Term1 = [_|_], Term2 = [_].


example1([5,3,5,7,13]).

example2([4,2,5,4,11]).

example3([5,3,5,7,49]).



test1 :-example1(List),equation(List).

test2 :-example2(List),equation(List).

test3 :-example3(List),equation(List).

testfile:-inputpath(Inpath),my_read_file(Inpath,L),equation(L).

test:-test1,write('\n\n'),test2,write('\n\n'),test3.


solve(List):-equation(List).