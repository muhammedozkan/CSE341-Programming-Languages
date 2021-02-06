% CSE341 Programming Languages HW4
% Muhammed ÖZKAN 151044084

%PART2

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

distance(istanbul,izmir,328).
distance(istanbul,ankara,351).
distance(istanbul,antalya,482).
distance(istanbul,gaziantep,847).
distance(istanbul,rize,967).
distance(istanbul,van,1262).

distance(edirne,edremit,235).

distance(edremit,edirne,235).
distance(edremit,erzincan,1066).

distance(erzincan,edremit,1066).

distance(izmir,isparta,308).
distance(izmir,istanbul,328).

distance(isparta,burdur,24).
distance(isparta,izmir,308).

distance(burdur,isparta,24).

distance(antalya,konya,192).
distance(antalya,istanbul,482).
distance(antalya,gaziantep,592).

distance(gaziantep,antalya,592).
distance(gaziantep,istanbul,847).

distance(konya,antalya,192).
distance(konya,ankara,227).

distance(ankara,konya,227).
distance(ankara,istanbul,351).
distance(ankara,van,920).

distance(van,rize,373).
distance(van,ankara,920).
distance(van,istanbul,1262).

distance(rize,van,373).
distance(rize,istanbul,967).

%rules
sroute(C1,C2,D) :- way(C1,C2,[C1],_,D),!.
way(C1,C2,P,[C2|P],D) :- flight(C1,C2),distance(C1,C2,D),!.
way(C1,C2,V,Path,D) :- flight(C1,M),M \== C2,\+member(M,V), distance(C1,M,Old),way(M,C2,[M|V],Path,D2), D is Old+ D2.