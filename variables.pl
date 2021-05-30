:- module(variables, 
	[
		price_list/1, visit_time_list/1, climate_list/1, children_activities_list/1,
        tourists_visiting_list/1, age_limit_list/1, meal_list/1, rooms_list/1,
        toilets_list/1, parking_list/1, adjusted_for_disabled_list/1
	]).


price_list([ cheap, medium, expensive]).
visit_time_list([short, medium, long]).
climate_list([kontynentalny, górski, morski]).
children_activities_list([few, some, many]).
tourists_visiting_list([few, some, many]).
age_limit_list([brak, nastolatkowie, z_rodzicem, dorosli]).
meal_list([not_important, important]).
rooms_list([not_important, important]).
toilets_list([not_important, important]).
parking_list([not_important, important]).
adjusted_for_disabled_list([not_important, important]).

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

print_variables([]) :- 
	format('~n').

print_variables([Head|Rest]) :-
	(
		Rest == [] -> format('~w',Head);
		format('~w, ',Head)
	),
	print_variables(Rest).