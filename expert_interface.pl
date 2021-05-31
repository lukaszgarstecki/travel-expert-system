:- module(expert_interface, [expert_menu/0]).

:- use_module(variables).
:- use_module(database_service).
:- use_module(reasoning).

expert_menu :-
    format('~nWhat do you want to do?~n~n'),
    print_variables([insert, update, delete]),
    read(Opt),
    (
        Opt == insert -> insert_interface;
        Opt == update -> update_interface;
        Opt == delete -> delete_interface
    ).

update_interface :-
    format('~nYou are updating existing place.~n'),
    read_place(Place),
    read_answers(Answers),
    format('~nDo you really want to update the place?~n'),
    confirm,
    update_place(Place, Answers),
    format('~nPlace was successfully updated.~n').


insert_interface :-
    format('~nYou are inserting new place.~n'),
    read_place(Place),
    read_answers(Answers),
    format('~nDo you really want to add the place?~n'),
    confirm,
    insert_place(Place, Answers),
    format('~nPlace was successfully inserted.~n').

delete_interface :-
    format('~nYou are deleting existing place.~n'),
    read_place(Place),
    format('~nDo you really want to delete the place?~n'),
    confirm,
    delete_place(Place),
    format('~nPlace was successfully deleted.~n').

confirm :-
    read_boolean(Opt),
    (
        Opt == yes -> true;
        format('~nOperation cancelled.~n'), false
    ).

read_answers([
    Price, VisitTime, Climate, ActivitiesForChildren,
    TouristsVisiting, AgeLimit, HasMeal, HasRoomsToSleep,
    HasToilet, HasParking, AdjustedForDisabled
]) :-
    read_price(Price),
    read_visit_time(VisitTime),
    read_climate(Climate),
    read_activities_for_children(ActivitiesForChildren),
    read_tourists_visiting(TouristsVisiting),
    read_age_limit(AgeLimit),
    read_has_meal(HasMeal),
    read_has_rooms_to_sleep(HasRoomsToSleep),
    read_has_toilet(HasToilet),
    read_has_parking(HasParking),
    read_adjusted_for_disabled(AdjustedForDisabled).

read_place(Name) :-
    format('~nWhat is the place name?~n'),
    read(Name).

read_price(Price) :-
    format('~nHow much costs one persons visit in the place (in PLN)?~n'),
    read(Price).

read_visit_time(VisitTime) :-
    format('~nHow long does the visit lasts (in minutes)?~n'),
    read(VisitTime).

read_climate(Climate) :-
    format('~nWhat is the climate?~n'),
    climate_list(Climates),
    print_variables(Climates),
    read(Climate).

read_activities_for_children(ActivitiesForChildren) :-
    format('~nHow many activities for children are avaliable?~n'),
    read(ActivitiesForChildren).

read_tourists_visiting(TouristsVisiting) :-
    format('~nHow may tourists visit the place each day?~n'),
    read(TouristsVisiting).

read_age_limit(AgeLimit) :-
    format('~nWhat is the age limit of the place?~n'),
    age_limit_list(AgeLimits),
    print_variables(AgeLimits),
    read(AgeLimit).

read_has_meal(HasMeal) :-
    format('~nDoes the place has meal included?~n'),
    read_boolean(HasMeal).

read_has_rooms_to_sleep(HasRoomsToSleep) :-
    format('~nDoes the place has rooms to sleep?~n'),
    read_boolean(HasRoomsToSleep).

read_has_toilet(HasToilet) :-
    format('~nDoes the place has toilet?~n'),
    read_boolean(HasToilet).

read_has_parking(HasParking) :-
    format('~nDoes the place has parking?~n'),
    read_boolean(HasParking).

read_adjusted_for_disabled(AdjustedForDisabled) :-
    format('~nDoes the place is adjusted for disabled?~n'),
    read_boolean(AdjustedForDisabled).

read_boolean(Result) :-
    yes_no_list(YesNo),
    print_variables(YesNo),
    read(Result).
