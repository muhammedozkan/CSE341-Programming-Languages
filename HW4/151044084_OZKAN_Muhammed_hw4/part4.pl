% CSE341 Programming Languages HW4
% Muhammed ÖZKAN 151044084

%PART4

%rules
element(X,L):- member(X,L),!.

union(L,S,U) :- helperUnion(L,S,U).
helperUnion([],R,R).
helperUnion([X|L],S,U) :- element(X,S),helperUnion(L,S,U),!. 
helperUnion([X|L],S,[X|U]) :- not(element(X,S)),helperUnion(L,S,U). 

intersect(L1,L2,I) :- helperIntersect(L1,L2,I).
helperIntersect([],_,[]). 
helperIntersect([X|L1],L2,[X|I]) :- element(X,L2),helperIntersect(L1,L2,I),!. 
helperIntersect([_|L1],L2,I) :- helperIntersect(L1,L2,I). 

equivalent(L1, L2) :- sort(L1, Srt1), sort(L2, Srt2), Srt1 == Srt2.