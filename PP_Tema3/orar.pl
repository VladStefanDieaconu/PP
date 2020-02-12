
:-ensure_loaded('probleme.pl').
:-ensure_loaded('testing.pl').

get_days(Context, Days) :- member(days(Days), Context).
get_times(Context, Times) :- member(times(Times), Context).
get_rooms(Context, Rooms) :- member(rooms(Rooms), Context).
get_groups(Context, Groups) :- member(groups(Groups), Context).
get_activities(Context, Activities) :- member(activities(Activities), Context).
get_staff(Context, Staff) :- member(staff(Staff), Context).
get_constraints(Context, Constraints) :- member(constraints(Constraints), Context).


% schedule(+Context, -Sol)
% pentru contextul descris, întoarce o soluție care respectă
% constrângerile fizice și de curiculă.
schedule(_, _) :- fail, !.
schedule(Context, (Context, [])):- get_activities(Context, []),
		%write('##nicio activitate de programat!\n'),
		!.
schedule(Context, (Context, [])):- get_activities(Context, [_|_]), get_staff(Context, []),
		%write('##no staff at all!\n'),
		fail, !.
schedule(Context, (Context, Sol)) :- 
	get_groups(Context, Groups),
	get_rooms(Context, Rooms),
	get_times(Context, Times),
	get_activities(Context, Activities),
	Activities \= [],
	get_staff(Context, Staff),
	get_days(Context, Days),
	makeList(Days, Times, Rooms, Groups, Activities, Staff, Sol), different(Sol),
	(
		\+ member(constraints(_), Context) ;
		(get_constraints(Context, Constraints), check_constraints(Days, Groups, Constraints, Sol))
	).
 
check_constraints(_, _, [], _).
check_constraints(Days, Groups, Constraints, Sol) :- 
	%(Constraints), write('\n'),
	check_max_instances_constraints(Days, Groups, Constraints, Sol).

check_max_instances_constraints(_, _, AllConstraints, _):- \+ member(max_instances(_), AllConstraints).
check_max_instances_constraints(Days, Groups, AllConstraints, Sol):-
		%trace,
		% write(AllConstraints),
		member(max_instances(_), AllConstraints),
		forall((member(max_instances(Course, N), AllConstraints), member(D, Days), member(G, Groups)),
					(
						% findall(slot(Course, G, D, T, R, P), member(slot(Course, G, D, T, R, P), Sol), L),
						aggregate_all(count, member(slot(Course, G, D, _, _, _), Sol), Count),
						% length(L, Lungime),
						 N > Count
					)
				).



% slot(A, G, D, T, R, P), unde
% A este numele activității
% G este numele grupei
% D este numele zilei
% T este perechea (nume interval, durată)
% R este numele sălii
% P este numele persoanei

makeList([], _, _, _, _, _, []) :- fail, !.
makeList(_, [], _, _, [_|_], _, []) :- fail, !.
makeList(_, _, [], _, _, _, []) :- fail, !.
makeList(_, _, _, [], _, _, []).
makeList(_, _, _, _, [], _, []).
% makeList(_, _, _, _, [_|_], [], []).

makeList(Days, Times, Rooms, [G|Groups], Activities, Staff, Sol) :-
	length(Activities, La), La \= 0,
	length(Staff, Ls), Ls \= 0,
	makeListForGroup(Days, Times, Rooms, G, Activities, Staff, SlotsForGroup),
	makeList(Days, Times, Rooms, Groups, Activities, Staff, SolRest),
	append(SlotsForGroup, SolRest, Sol).
	% flatten(All, Sol).

makeSlotsForActivity(Days, Times, Rooms, G, A, Staff, AllPossibleSlots):-
	setof(slot(A, G, D, T, R, P), D^T^R^P^(
		member((P, Materii), Staff), member(A, Materii),
		member(D, Days),
		member(T, Times),
		member(R, Rooms)
	), AllPossibleSlots ).

subset2([],[]).
subset2([X|L],[X|S]) :-
	subset2(L,S).
subset2(L, [_|S]) :-
	subset2(L,S).

makeListForGroup(_, _, _, _, [], _, []).

makeListForGroup(Days, Times, Rooms, G, [(A, NrInst) | Activities], Staff, SolutionForGroup) :-
	makeSlotsForActivity(Days, Times, Rooms, G, A, Staff, Slots),
	subset2(SubSetOfSlots, Slots),
	length(SubSetOfSlots, NrInst),
    makeListForGroup(Days, Times, Rooms, G, Activities, Staff, Rest),
	append(SubSetOfSlots, Rest, SolutionForGroup).


% constrângeri fizice
different([slot(_, Group, Day, Time, Room, Teacher) | Rest]) :- 
    \+ member(slot(_, _, Day, Time, _, Teacher), Rest),
    \+ member(slot(_, _, Day, Time, Room, _), Rest),
    \+ member(slot(_, Group, Day, Time, _, _), Rest),
    different(Rest).
different([]).


% cost(+Sol, -Cost)
% pentru soluția dată, întoarce costul implicat de constrângerile de
% preferință care au fost încălcate.
% cost(_, _) :- fail.
cost((_, []), 0).
cost((Context, Sol), Cost):-
	get_groups(Context, Groups),
	get_rooms(Context, Rooms),
	get_times(Context, Times),
	get_staff(Context, Staff),
	get_days(Context, Days),
	(
		\+ member(constraints(_), Context) ;
		(get_constraints(Context, Constraints), check_cost_constraints(Days, Times, Rooms, Groups, Staff, Constraints, Sol, Cost))
	).
 
check_cost_constraints(_, _, _, _, [], _, 0).
check_cost_constraints(Days, Times, Rooms, Groups, Staff, Constraints, Sol, Cost) :- 
	Constraints \= [],
	check_max_hours_constraints(Days, Times, Rooms, Groups, Staff, Constraints, Sol, Cost).

check_max_hours_constraints(_, _, _, _, _, Constraints, _, 0):- 
				% \+ member(max_hours(_, _, _), Constraints).
				% trace,
				debug,
				\+ member(max_hours(_), Constraints).
check_max_hours_constraints(Days, [(_, Duration)|_], Rooms, Groups, _, Staff, Constraints, Sol, TotalCost):-
		% member(max_hours(_, _, _), Constraints),
		findall(ThisCost, (
							member(max_hours(Entity, Hours, Cost), Constraints),
							member(D, Days),
							(
								% slot(Course, G, D, T, R, P)
								(
									member(Entity, Groups),
									aggregate_all(count, member(slot(_, Entity, D, _, _, _), Sol), Count)
								) ;
								(
									member(Entity, Staff),
									aggregate_all(count, member(slot(_, _, D, _, _, Entity), Sol), Count)
								) ;
								(
									member(Entity, Rooms),
									aggregate_all(count, member(slot(_, _, D, _, Entity, _), Sol), Count)
								) 
							),
							(
								(Hours >= Count * Duration, ThisCost is 0) ;
								(ThisCost is ((Count * Duration - Hours) * Cost))
							)

						), 
				AllCosts),
		sumlist(AllCosts, TotalCost).

% schedule_best(+Context, -Sol, -Cost)
% pentru contextul descris, întoarce soluția validă cu cel mai bun (cel
% mai mic) cost (sau una dintre ele, dacă există mai multe cu același
% cost)
schedule_best(_, _, _) :- fail.













