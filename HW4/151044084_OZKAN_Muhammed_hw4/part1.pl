% CSE341 Programming Languages HW4
% Muhammed ÖZKAN 151044084

%PART1

%knowledgebase
flight(istanbul,antalya).
flight(istanbul,izmir).
flight(istanbul,gaziantep).
flight(istanbul,ankara).
flight(istanbul,van).
flight(istanbul,rize).

flight(edirne,edremit).

flight(edremit,edirne).
flight(edremit,erzincan).

flight(erzincan,edremit).

flight(izmir,istanbul).
flight(izmir,isparta).

flight(isparta,izmir).
flight(isparta,burdur).

flight(burdur,isparta).

flight(antalya,istanbul).
flight(antalya,konya).
flight(antalya,gaziantep).

flight(gaziantep,istanbul).
flight(gaziantep,antalya).

flight(konya,antalya).
flight(konya,ankara).

flight(ankara,istanbul).
flight(ankara,konya).
flight(ankara,van).

flight(van,istanbul).
flight(van,ankara).
flight(van,rize).

flight(rize,van).
flight(rize,istanbul).

%rules
:- dynamic visited/1.
route(C1,C2) :- retractall(visited(_)),helperoute(C1,C2),print(C2),nl, fail.
helperoute(C1,C2) :- flight(C1,C2).
helperoute(C1,C2) :- flight(C1,C3),not(visited(C3)),assert(visited(C3)),helperoute(C3,C2),not(C2==C1).