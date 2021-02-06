% CSE341 Programming Languages HW4
% Muhammed ÖZKAN 151044084

%PART3

%facts
when(102,10).
when(108,12).
when(341,14).
when(455,16).
when(452,17).

where(102,z23).
where(108,z11).
where(341,z06).
where(455,207).
where(452,207).

enroll(a,102).
enroll(a,108).
enroll(b,102).
enroll(c,108).
enroll(d,341).
enroll(e,455).

%rules
schedule(S,P,T):- enroll(S,C),where(C,P), when(C,T).

usage(P,T):- where(C,P), when(C,T).

conflict(C1,C2):- not((not(helperclass(C1,C2)),not(helpertime(C1,C2)))).
helperclass(C1,C2):- where(C1,A), where(C2,B), A == B.
helpertime(C1,C2) :-  when(C1,A), when(C2,B), A == B.

meet(X,Y):- enroll(X,C1),where(C1,P1), enroll(Y,C2),where(C2,P2), C1==C2,P1==P2,!.