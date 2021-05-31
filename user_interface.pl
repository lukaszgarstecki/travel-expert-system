:- module(user_interface, [read_answers/1, print_answers/1]).

:- use_module(variables).

read_answers([ Price, VisitTime, Climate, ChildrenActivity, TouristsVisiting,
        AgeLimit, Meal, Room, Toilet, Parking, AdjsutedForDisabled]) :-
    read_price_range(Price),
    read_visit_time(VisitTime),
    read_climate(Climate),
    read_children_activity(ChildrenActivity),
    read_tourists_visiting(TouristsVisiting),
    read_age_limit(AgeLimit),
    read_meal(Meal),
    read_room(Room),
    read_toilet(Toilet),
    read_parking(Parking),
    read_adjusted_for_disabled(AdjsutedForDisabled).

read_price_range(Price) :-
    format('~nHow expensive should your place be?~n'),
    price_list(Prices),
    print_variables(Prices),
    read(Price).

read_visit_time(VisitTime) :-
    format('~nHow long are you going to visit the place?~n'),
    visit_time_list(VisitTimes),
    print_variables(VisitTimes),
    read(VisitTime).

read_climate(Climate) :-
    format('~nWhat climate do you prefer?~n'),
    climate_list(Climates),
    print_variables(Climates),
    read(Climate).

read_children_activity(ChildrenActivity) :-
    format('~nHow many children activities do you expect?~n'),
    children_activities_list(ChildrenActivities),
    print_variables(ChildrenActivities),
    read(ChildrenActivity).

read_tourists_visiting(TouristsVisiting) :-
    format('~nHow many tourists do you expect to meet?~n'),
    tourists_visiting_list(TouristsVisitingOpts),
    print_variables(TouristsVisitingOpts),
    read(TouristsVisiting).

read_age_limit(AgeLimit) :-
    format('~nWhat age limitations do you expect?~n'),
    age_limit_list(AgeLimits),
    print_variables(AgeLimits),
    read(AgeLimit).

read_meal(Meal) :-
    format('~nDo you expect a meal?~n'),
    meal_list(Meals),
    print_variables(Meals),
    read(Meal).

read_room(Room) :-
    format('~nDo you expect a place to sleep?~n'),
    rooms_list(Rooms),
    print_variables(Rooms),
    read(Room).

read_toilet(Toilet) :-
    format('~nDo you expect to have a toilet?~n'),
    toilets_list(Toilets),
    print_variables(Toilets),
    read(Toilet).

read_parking(Parking) :-
    format('~nDo you expect to have a parking space?~n'),
    parking_list(Parkings),
    print_variables(Parkings),
    read(Parking).

read_adjusted_for_disabled(AdjsutedForDisabled) :-
    format('~nDo you expect place beeing adjusted for disabled?~n'),
    adjusted_for_disabled_list(AdjsutedForDisabledOpts),
    print_variables(AdjsutedForDisabledOpts),
    read(AdjsutedForDisabled).

print_answers([]) :-
    format('~n').

print_answers([Head|Rest]) :-
    (
        Rest == [] -> format('~w',Head);
        format('~w, ',Head)
    ),
    print_variables(Rest).
