
problem(zero, [days([lu, ma, mi]),
          times([(8-10, 2)]),
          rooms([eg105, an110, eg104]),
          groups([grupa1]),
          activities([]),
          staff([])]) :- !.

problem(unu1, [days([lu, ma]),
          times([(8-10, 2), (10-12, 2)]),
          rooms([sala, afara]),
          groups([grupa1]),
          activities([(sport, 1)]),
          staff([
              (marcel, [sport])
          ])]) :- !.
problem(unu2, [days([lu, ma]),
          times([(8-10, 2), (10-12, 2)]),
          rooms([sala, afara]),
          groups([grupa1]),
          activities([(sport, 1), (karate, 1)]),
          staff([
              (marcel, [sport]),
              (kobayashi, [karate])
          ])]) :- !.
problem(unu3, [days([lu]),
          times([(8-10, 2), (10-12, 2)]),
          rooms([sala, afara]),
          groups([grupa1, grupa2]),
          activities([(sport, 1), (karate, 1)]),
          staff([
              (marcel, [sport]),
              (kobayashi, [karate])
          ])]) :- !.

problem(no_staff, [
	days([lu, ma, mi]),
	times(TT),
	groups([grupa1, grupa2]),
	rooms([eg105, eg104]),
	activities([(cpl, 1), (pp, 6)]),
	staff([
		(mihnea, [pp]),
		(mihaela, [pp]),
		(andrei, [lfa])
	])
]) :- intervals(8, 16, 2, TT), !.

problem(no_time, [
	days([lu, ma]),
	times(TT),
	groups([grupa1, grupa2]),
	rooms([eg105, eg104]),
	activities([(cpl, 1), (pp, 6)]),
	staff([
		(mihnea, [pp, cpl]),
		(mihaela, [pp]),
		(andrei, [lfa])
	])
]) :- intervals(8, 12, 2, TT), !.

problem(no_room, [
	days([lu, ma]),
	times(TT),
	groups([grupa1, grupa2]),
	rooms([eg105]),
	activities([(cpl, 1), (pp, 6)]),
	staff([
		(mihnea, [pp]),
		(mihaela, [pp]),
		(andrei, [lfa])
	])
]) :- intervals(8, 16, 2, TT), !.

% ===========================================

problem(sameSmall, [
            days([lu]),
            times(TT),
            groups([grupa]),
            rooms([sala]),
            activities([(pp, 2)]),
            staff([(mihnea, [pp])])
        ]) :- intervals(8, 10, 1, TT), !.

problem(sameBigNone, [
	activities([(cpl, 1), (ppC, 1), (ppL, 1), (lfaC, 1), (lfaS, 1)]),
	days([lu, ma, mi, jo, vi]),
	times(TT),
	groups([grupa1]),
	rooms([eg105]),
	staff([
		(mihnea, [cpl]),
		(mihaela, [ppC, ppL]),
		(andrei, [lfaC, lfaS])
	])
]) :- intervals(8, 10, 2, TT), !.

problem(sameBigSome, [
	activities([(cpl, 1), (pp, 2), (lfa, 2)]),
	days([lu, ma, mi, jo, vi]),
	times(TT),
	groups([grupa1]),
	rooms([eg105]),
	staff([
		(mihnea, [cpl]),
		(mihaela, [pp]),
		(andrei, [lfa])
	])
]) :- intervals(8, 10, 2, TT), !.


% ===========================================

problem(maxi, [
	days([lu, ma]),
	times(TT),
	groups([grupa1, grupa2]),
	rooms([eg105]),
	activities([(pp, 3)]),
	staff([
		(mihaela, [pp])
	]),
	constraints([
		max_instances(pp, 2)
	])
]) :- intervals(8, 16, 2, TT), !.

problem(maxiNo, [
	days([lu, ma]),
	times(TT),
	groups([grupa1, grupa2]),
	rooms([eg105]),
	activities([(pp, 6)]),
	staff([
		(mihaela, [pp])
	]),
	constraints([
		max_instances(pp, 2)
	])
]) :- intervals(8, 16, 2, TT), !.

problem(maxi2, [
	days([lu, ma, mi]),
	times(TT),
	groups([grupa1]),
	rooms([eg105]),
	activities([(pp, 4), (cpl, 5)]),
	staff([
            (mihaela, [pp]),
            (mihnea, [cpl])
	]),
	constraints([
            max_instances(pp, 2),
            max_instances(cpl, 2)
	])
]) :- intervals(8, 14, 2, TT), !.


% ===========================================

problem(minmax1, [days([lu]),
          times([(8-9, 1), (9-10, 1), (10-11, 1), (11-12, 1)]),
          rooms([sala]),
          groups([grupa1]),
          activities([(sport, 2), (karate, 2)]),
          staff([
              (marcel, [sport, karate]),
              (kobayashi, [sport, karate])
          ])]) :- !.

problem(minmax2, [
	days([lu, ma]),
	times(TT),
	rooms([eg105, eg104]),
	groups([grupa1, grupa2]),
	staff([
		(andrei, [ia, lfa, pp]),
		(mihnea, [cpl, pp]),
		(tudor, [ia, pp])
	]),
	activities([(pp, 3), (ia, 2)])%,
%	constraints([
%		min_hours(tudor, 4, 1),
%		max_hours(andrei, 2, 5)
%	])
]) :- intervals(8, 20, 2, TT), !.

problem(minmax2, [
	days([lu, ma]),
	times(TT),
	rooms([eg105, eg104]),
	groups([grupa1, grupa2]),
	staff([
		(andrei, [ia, lfa, pp]),
		(mihnea, [cpl, pp]),
		(tudor, [ia, pp])
	]),
	activities([(pp, 3), (ia, 2)])
]) :- intervals(8, 20, 2, TT), !.

problem(continuums, [
	days([lu, ma, mi]),
	times(TT),
	rooms([eg105]),
	groups([grupa1, grupa2]),
	staff([
		(andrei, [pp])
	]),
	activities([(pp, 8)])
]) :- intervals(8, 20, 2, TT), !.

problem(continuums2, [
	days([lu, ma]),
	times(TT),
	rooms([eg105]),
	groups([grupa1, grupa2]),
	staff([
		(andrei, [pp]), (mihnea, [cpl])
	]),
	activities([(pp, 3), (cpl, 3)])
]) :- intervals(8, 20, 2, TT), !.


% ===========================

problem(maxiBest, [
	days([lu, ma, mi]),
	times(TT),
	groups([grupa1]),
	rooms([eg105]),
	activities([(pp, 6), (cpl, 3)]),
	staff([
            (mihaela, [pp]),
            (mihnea, [cpl])
	]),
	constraints([
            max_instances(pp, 2),
            interval(mihaela, 10, 14, 2),
            min_hours(mihnea, 4, 10),
            continuous(mihnea, 5)
	])
]) :- intervals(8, 14, 2, TT), !.

problem(maxiBestZero, [
	days([lu, ma, mi]),
	times(TT),
	groups([grupa1]),
	rooms([eg105]),
	activities([(pp, 6), (cpl, 3)]),
	staff([
            (mihaela, [pp]),
            (mihnea, [cpl])
	]),
	constraints([
            max_instances(pp, 2),
            interval(mihaela, 10, 14, 2),
            continuous(mihnea, 5)
	])
]) :- intervals(8, 14, 2, TT), !.

problem(continuumsBest, [
	days([lu, ma]),
	times(TT),
	rooms([eg105,eg104]),
	groups([grupa1, grupa2]),
	staff([
		(andrei, [pp]), (mihnea, [cpl])
	]),
	activities([(pp, 2), (cpl, 2)]),
        constraints([
            interval(eg104, 8, 10, 1),
            interval(eg105, 12, 14, 1),
            interval(mihnea, 8, 10, 1),
            interval(andrei, 12, 14, 1),
            interval(grupa2, 8, 10, 1),
            interval(grupa1, 12, 14, 1)
        ])
]) :- intervals(8, 14, 2, TT), !.

% ===========================

problem(hard, [
	days([lu, ma, mi]),
	times(TT),
	rooms([eg105, eg104, an110]),
	groups([grupa1]),
	staff([
		(andrei, [ai, ia, lfa, pp]),
		(mihnea, [cpl, pp]),
		(mihaela, [pa, pp])
	]),
	activities([(ia, 3), (lfa, 2), (pa, 2), (pp, 2)]),
	constraints([
		max_instances(pp, 2),
		min_hours(andrei, 4, 1),
		max_hours(mihnea, 2, 1),
		continuous(andrei, 100),
		continuous(mihnea, 100),
		continuous(mihaela, 100),
		interval(mihnea, 8, 12, 200),
		interval(andrei, 10, 16, 200)
	])
]) :- intervals(8, 20, 2, TT), !.

problem(allZero, [
	days([lu, ma, mi]),
	times(TT),
	rooms([sala1, sala2, sala2, sala4]),
	staff([
		(popa, [mate]),
		(ion, [info]),
		(george, [fizica])
	]),
	activities([(mate, 5), (info, 4), (fizica, 2)]),
	groups([clasaIXA, clasaIXB, clasaIXC]),
	constraints([
		max_instances(mate, 3),
		max_instances(fizica, 1),
		max_instances(info, 2),
		continuous(popa, 3),
		continuous(george, 97),
%		max_hours(popa, 4, 3),
		max_hours(ion, 4, 3),
		max_hours(george, 4, 3),
		min_hours(ion, 2, 4),
		interval(ion, 8, 12, 3)
	])
]) :- intervals(8, 13, 1, TT), !.

problem(all, [
	days([lu, ma, mi]),
	times(TT),
	rooms([sala1, sala2, sala2, sala4]),
	staff([
		(popa, [mate]),
		(ion, [info]),
		(george, [fizica])
	]),
	activities([(mate, 5), (info, 4), (fizica, 2)]),
	groups([clasaIXA, clasaIXB, clasaIXC]),
	constraints([
		max_instances(mate, 3),
		max_instances(fizica, 1),
		max_instances(info, 2),
		continuous(popa, 3),
		continuous(george, 97),
		max_hours(popa, 4, 3),
		max_hours(ion, 4, 3),
		max_hours(george, 4, 3),
		min_hours(ion, 2, 4),
		interval(ion, 8, 12, 3)
	])
]) :- intervals(8, 13, 1, TT), !.

problem(Name, _) :- format('INTERN: problema ~w nu exista.~n', [Name]), fail.

intervals(From, To, _Size, []) :- From >= To, !.
intervals(From, To, Size, [(From-Next, Size)|Output]) :-
    Next is From + Size,
    intervals(Next, To, Size, Output).



screenWidth(80).
colWidth(List, CW) :- length(List, NC), screenWidth(WW), CW is round(WW / NC).
printTabbed(List) :- colWidth(List, W), format("|"),
    forall(nth1(I, List, E), (Tab is I * W, format('~t~w~t~*||', [E, Tab]))), nl.


printSched((PD, S)) :- member(groups(Groups), PD),
    member(days(Days), PD), member(times(Times), PD),
    GroupsPlus = [""|Groups], printTabbed(GroupsPlus),
    forall((member(D, Days), member(T, Times)), (
               findall(GP, (member(G, Groups),
                            findall(A:P:R, member(slot(A,G,D,T,R,P), S), GP)), GPS),
               findall(Len, (member(GP, GPS), length(GP, Len)), Lens), max_list(Lens, H),
               printSched(PD, GPS, H, [], [D:T])
           )).
printSched(PD, _, 0, _, [D:T]) :- !,
    member(groups(Groups), PD),
    findall('', member(_, Groups), L), printTabbed([D:T|L]).
printSched(_, _, 0, _, _).
printSched(PD, [], H, DO, Line) :- !, H1 is H - 1,
    reverse(Line, L), printTabbed(L),
    reverse(DO, RDO), printSched(PD, RDO, H1, [], [""]).
printSched(PD, [[]|DSS], H, DO, Line) :- !, printSched(PD, DSS, H, [[]|DO], [""|Line]).
printSched(PD, [[E|DS]|DSS], H, DO, Line) :- !, printSched(PD, DSS, H, [DS|DO], [E|Line]).


sol(Pb) :- problem(Pb, PD), schedule(PD, Sol),
    printSched(Sol), Sol=(_, Sch), write(Sch), nl, write('-----------'), nl.
sols(Pb) :- problem(Pb, PD),
    findall(Sol, schedule(PD, Sol), Sols), length(Sols, N),
    format('===========================~n ~w solutii ~n ===================== ~n', [N]),
    forall(member(Sol, Sols), (printSched(Sol), Sol=(_, Sch),
                               write(Sch), nl, write('-----------'), nl)),
    format('===========================~n ~w solutii ~n ===================== ~n', [N]).
nsols(Pb) :- problem(Pb, PD),
    findall(Sol, schedule(PD, Sol), Sols), length(Sols, N),
    format('===========================~n ~w solutii ~n ===================== ~n', [N]).
bestsol(Pb) :- problem(Pb, PD), schedule_best(PD, Sol, C),
	Sol=(_, Sch), write(Sch), nl, format('----------- Cost ~w~n', [C]),
    printSched(Sol).
