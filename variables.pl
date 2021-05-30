:- module(variables, 
	[
		price_list/1, visit_time_list/1, climate_list/1, children_activities_list/1,
        tourists_visiting_list/1, age_limit_list/1, meal_list/1, rooms_list/1,
        toilets_list/1, parking_list/1, adjusted_for_disabled_list/1, yes_no_list/1,
		map_price_to_price_factor/2, map_visit_time_to_factor/2,
		map_children_activities_range_to_factor/2,
		map_tourists_visiting_range_to_factor/2,
		map_meals/2, map_rooms/2, map_toilets/2, map_parking/2,
		map_adjusted_for_disabled/2,
		print_variables/1
	]).


price_list([ cheap, medium, expensive]).
visit_time_list([short, medium, long]).
climate_list([kontynentalny, górski, morski]).
children_activities_list([few, some, many]).
tourists_visiting_list([few, some, many]).
age_limit_list([brak, nastolatkowie, z_rodzicem, dorosli]).
meal_list(List) :-
	yes_no_list(List).
rooms_list(List) :-
	yes_no_list(List).
toilets_list(List) :-
	yes_no_list(List).
parking_list(List) :-
	yes_no_list(List).
adjusted_for_disabled_list(List) :-
	yes_no_list(List).
yes_no_list([yes, no]).

map_price_to_price_factor(Price, Factor) :-
	(
		Price == cheap -> Factor is 0;
		Price == medium -> Factor is 0.5;
		Price == expensive -> Factor is 1
	).

map_visit_time_to_factor(Time, Factor) :-
	(
		Time == short -> Factor is 0;
		Time == medium -> Factor is 0.5;
		Time == long -> Factor is 1
	).

map_children_activities_range_to_factor(Quantity, Factor) :-
	(
		Quantity == few -> Factor is 0;
		Quantity == some -> Factor is 0.5;
		Quantity == many -> Factor is 1
	).

map_tourists_visiting_range_to_factor(Quantity, Factor) :-
	(
		Quantity == few -> Factor is 0;
		Quantity == some -> Factor is 0.5;
		Quantity == many -> Factor is 1
	).

map_meals(Answer, Factor) :-
	(
		Answer == no -> Factor is 0;
		Answer == yes -> Factor is 1
	).

map_rooms(Answer, Factor) :-
	(
		Answer == no -> Factor is 0;
		Answer == yes -> Factor is 1
	).

map_toilets(Answer, Factor) :-
	(
		Answer == no -> Factor is 0;
		Answer == yes -> Factor is 1
	).

map_parking(Answer, Factor) :-
	(
		Answer == no -> Factor is 0;
		Answer == yes -> Factor is 1
	).

map_adjusted_for_disabled(Answer, Factor) :-
	(
		Answer == no -> Factor is 0;
		Answer == yes -> Factor is 1
	).

print_variables([]) :- 
	format('~n').

print_variables([Head|Rest]) :-
	(
		Rest == [] -> format('~w',Head);
		format('~w, ',Head)
	),
	print_variables(Rest).