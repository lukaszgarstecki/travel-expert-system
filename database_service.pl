:- module(database_service, [update_place/2, insert_place/2, delete_place/1]).
:- use_module(db).

update_place(Place, Attributes) :-
    delete_place(Place),
    insert_place(Place, Attributes),
	commit.


insert_place(
    Place,
    [
        Price, VisitTime, Climate, ActivitiesForChildren,
        TouristsVisiting, AgeLimit, HasMeal, HasRoomsToSleep,
        HasToilet, HasParking, AdjustedForDisabled
    ]) :-

    assertz(db:price(Place,Price)),
    assertz(db:visit_time(Place,VisitTime)),
    assertz(db:climate(Place,Climate)),
    assertz(db:activities_for_children(Place,ActivitiesForChildren)),
    assertz(db:tourists_visiting(Place,TouristsVisiting)),
    assertz(db:age_limit(Place,AgeLimit)),
    (
        HasMeal == yes -> assertz(db:has_meal(Place));
        true
    ),
    (
        HasRoomsToSleep == yes -> assertz(db:has_rooms_to_sleep(Place));
        true
    ),
    (
        HasToilet == yes -> assertz(db:has_toilet(Place));
        true
    ),
	(
        HasParking == yes -> assertz(db:has_parking(Place));
        true
    ),
	(
        AdjustedForDisabled == yes -> assertz(db:adjusted_for_disabled(Place));
        true
    ),
	commit.

delete_place(Place) :-
	retractall(db:price(Place,_)),
	retractall(db:visit_time(Place,_)),
	retractall(db:climate(Place, _)),
	retractall(db:activities_for_children(Place,_)),
	retractall(db:tourists_visiting(Place,_)),
	retractall(db:age_limit(Place, _)),
	retractall(db:has_meal(Place,_)),
	retractall(db:has_rooms_to_sleep(Place)),
	retractall(db:has_toilet(Place, _)),
	retractall(db:has_parking(Place)),
	retractall(db:adjusted_for_disabled(Place)),
	commit.


commit :-
    tell('places_db.pl'),
    listing(db:price),
    listing(db:visit_time),
    listing(db:climate),
    listing(db:activities_for_children),
    listing(db:tourists_visiting),
    listing(db:age_limit),
    listing(db:has_meal),
    listing(db:has_rooms_to_sleep),
    listing(db:has_toilet),
    listing(db:has_parking),
    listing(db:adjusted_for_disabled),
    told.
