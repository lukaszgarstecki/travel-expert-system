:- module(reasoning, 
    [
    price_factor/2,
    visit_time_factor/2,
    climate_factor/3,
    children_activities_factor/2,
    tourists_visiting_factor/2,
    age_limit_factor/3,
    has_meal_factor/3,
    has_rooms_too_sleep_factor/3,
    has_toilet_factor/3,
    has_parking_factor/3,
    adjusted_for_disabled_factor/3
    ]).

:- use_module(variables).
:- use_module(db).

/* Price Range */
price_factor(Place, Factor) :-
    cheap_price_range(Place, CheapFactor),
    medium_price_range(Place, MediumFactor),
    expensive_price_range(Place, ExpensiveFactor),
    map_price_to_price_factor(cheap, CheapMappedFactor),
    map_price_to_price_factor(medium, MediumMappedFactor),
    map_price_to_price_factor(expensive, ExpensiveMappedFactor),
    sharpen(
        [CheapFactor, MediumFactor, ExpensiveFactor],
        [CheapMappedFactor, MediumMappedFactor, ExpensiveMappedFactor],
        Factor
    ).

cheap_price_range(Place, Factor) :-
    db:price(Place, Price),
    (
        Price < 100 -> Factor is 1;
        Factor is 0
    ).

medium_price_range(Place, Factor) :-
    db:price(Place, Price),
    (
        Price >= 400, Price < 600 -> Factor is 1;
        Factor is 0
    ).

expensive_price_range(Place, Factor) :- 
    db:price(Place, Price),
    (
        Price >= 600 -> Factor is 1;
        Factor is 0
    ).

/* Visit time */
visit_time_factor(Place, Factor) :-
    short_visit_time(Place, ShortFactor),
    medium_visit_time(Place, MediumFactor),
    long_visit_time(Place, LongFactor),
    map_visit_time_to_factor(short, ShortMappedFactor),
    map_visit_time_to_factor(medium, MediumMappedFactor),
    map_visit_time_to_factor(long, LongMappedFactor),
    sharpen(
        [ShortFactor, MediumFactor, LongFactor],
        [ShortMappedFactor, MediumMappedFactor, LongMappedFactor],
        Factor
    ).

short_visit_time(Place, Factor) :-
    db:visit_time(Place, Time),
    (
        Time < 200 -> Factor is 1;
        Factor is 0
    ).

medium_visit_time(Place, Factor) :-
    db:visit_time(Place, Time),
    (
        Time >= 200, Time < 2000 -> Factor is 1;
        Factor is 0
    ).

long_visit_time(Place, Factor) :- 
    db:visit_time(Place, Time),
    (
        Time >= 2000 -> Factor is 1;
        Factor is 0
    ).

/* Climate */
climate_factor(Climate, Place, Factor) :-
(
    db:climate(Place, Climate) -> Factor is 1;
    Factor is 0
).

/* Activities for children */
children_activities_factor(Place, Factor) :-
    few_children_activities_range(Place, FewFactor),
    some_children_activities_range(Place, SomeFactor),
    many_children_activities_range(Place, ManyFactor),
    map_children_activities_range_to_factor(short, FewMappedFactor),
    map_children_activities_range_to_factor(medium, SomeMappedFactor),
    map_children_activities_range_to_factor(long, ManyMappedFactor),
    sharpen(
        [FewFactor, SomeFactor, ManyFactor],
        [FewMappedFactor, SomeMappedFactor, ManyMappedFactor],
        Factor
    ).

few_children_activities_range(Place, Factor) :-
    db:activities_for_children(Place, Quantity),
    (
        Quantity < 10 -> Factor is 1;
        Factor is 0
    ).

some_children_activities_range(Place, Factor) :-
    db:activities_for_children(Place, Quantity),
    (
        Quantity >= 10, Quantity < 20 -> Factor is 1;
        Factor is 0
    ).

many_children_activities_range(Place, Factor) :- 
    db:activities_for_children(Place, Quantity),
    (
        Quantity >= 20 -> Factor is 1;
        Factor is 0
    ).

/* Tourists visiting */
tourists_visiting_factor(Place, Factor) :-
    few_tourists_visiting_range(Place, FewFactor),
    some_tourists_visiting_range(Place, SomeFactor),
    many_tourists_visiting_range(Place, ManyFactor),
    map_tourists_visiting_range_to_factor(short, FewMappedFactor),
    map_tourists_visiting_range_to_factor(medium, SomeMappedFactor),
    map_tourists_visiting_range_to_factor(long, ManyMappedFactor),
    sharpen(
        [FewFactor, SomeFactor, ManyFactor],
        [FewMappedFactor, SomeMappedFactor, ManyMappedFactor],
        Factor
    ).

few_tourists_visiting_range(Place, Factor) :-
    db:tourists_visiting(Place, Quantity),
    (
        Quantity < 50 -> Factor is 1;
        Factor is 0
    ).

some_tourists_visiting_range(Place, Factor) :-
    db:tourists_visiting(Place, Quantity),
    (
        Quantity >= 50, Quantity < 500 -> Factor is 1;
        Factor is 0
    ).

many_tourists_visiting_range(Place, Factor) :- 
    db:tourists_visiting(Place, Quantity),
    (
        Quantity >= 500 -> Factor is 1;
        Factor is 0
    ).

/* Age limit version 3 */
age_limit_factor(AgeLimit, Place, Factor) :-
(
    db:age_limit(Place, AgeLimit) -> Factor is 1;
    Factor is 0
).

/* Meals */
has_meal_factor(Meals, Place, Factor) :-
(
    Meals == not_important -> no_meal_factor(Place, Factor);
    Meals == important -> meal_factor(Place, Factor)
).

no_meal_factor(Place, Factor) :- 
(
    Factor is 1
).

meal_factor(Place, Factor) :-
(
    db:has_meal(Place) -> Factor is 1;
    Factor is 0
).

/* Rooms to sleep */
has_rooms_too_sleep_factor(Rooms, Place, Factor) :-
(
    Rooms == not_important -> no_room_factor(Place, Factor);
    Rooms == important -> room_factor(Place, Factor)
).

no_room_factor(Place, Factor) :-
(
    Factor is 1
).

room_factor(Place, Factor) :-
(
    db:has_rooms_too_sleep(Place) -> Factor is 1;
    Factor is 0
).

/* Toilets */
has_toilet_factor(Toilets, Place, Factor) :-
(
    Toilets == not_important -> no_toilet_factor(Place, Factor);
    Toilets == important -> toilet_factor(Place, Factor)
).

no_toilet_factor(Place, Factor) :-
(
    Factor is 1
).

toilet_factor(Place, Factor) :-
(
    db:has_toilet(Place) -> Factor is 1;
    Factor is 0
).

/* Parking */
has_parking_factor(Parking, Place, Factor) :-
(
    Parking == not_important -> no_parking_factor(Place, Factor);
    Parking == important -> parking_factor(Place, Factor)
).

no_parking_factor(Place, Factor) :-
(
    Factor is 1
).

parking_factor(Place, Factor) :-
(
    db:has_parking(Place) -> Factor is 1;
    Factor is 0
).

/* Adjustment for disabled */
adjusted_for_disabled_factor(Adjustment, Place, Factor) :-
(
    Adjustment == not_important -> no_adjustment_for_disabled_factor(Place, Factor);
    Adjustment == important -> adjustment_for_disabled_factor(Place, Factor)
).

no_adjustment_for_disabled_factor(Place, Factor) :-
(
    Factor is 1
).

adjustment_for_disabled_factor(Place, Factor) :-
(
    db:adjusted_for_disabled(Place) -> Factor is 1;
    Factor is 0
).

/* Helpers */
triangle_down(Left, Right, Price, Factor) :-
    Factor is (Right - Price) / (Right - Left).

triangle_up(Left, Right, Price, Factor) :-
    Factor is (Price - Left) / (Right - Left).

sharpen(Factors, FactorMappedValues, Result) :-
    sum_prods_list(Factors, FactorMappedValues, Numerator),
    sum_list(Factors, Denominator),
    (
        Denominator == 0 -> Result is 0;
        Result is Numerator / Denominator
    ).

sum_prods_list([], [], 0).
sum_prods_list([H_left|T_left], [H_right|T_right], Product) :- 
    sum_prods_list(T_left, T_right, Rest), 
    Product is (H_left * H_right) + Rest.

sum_list([], 0).
sum_list([H|T], Sum) :-
   sum_list(T, Rest),
   Sum is H + Rest.