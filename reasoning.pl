:- module(reasoning, 
    [
    price_factor/2,
    visit_time_factor/2,
    climate_factor/3,
    children_activities_factor/2,
    tourists_visiting_factor/2,
    age_limit_factor/3,
    has_meal_factor/2,
    has_rooms_to_sleep_factor/2,
    has_toilet_factor/2,
    has_parking_factor/2,
    adjusted_for_disabled_factor/2
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
        Price >= 100, Price < 200 -> triangle_down(100, 200, Price, Factor);
        Factor is 0
    ).

medium_price_range(Place, Factor) :-
    db:price(Place, Price),
    (
        Price >= 100, Price < 200 -> triangle_up(100, 200, Price, Factor);
        Price >= 200, Price < 400 -> Factor is 1;
        Price >= 400, Price < 600 -> triangle_down(400, 600, Price, Factor);
        Factor is 0
    ).

expensive_price_range(Place, Factor) :- 
    db:price(Place, Price),
    (
        Price >= 400, Price < 600 -> triangle_up(400, 600, Price, Factor);
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
        Time < 300 -> Factor is 1;
        Time >= 300, Time < 600 -> triangle_down(300, 600, Time, Factor);
        Factor is 0
    ).

medium_visit_time(Place, Factor) :-
    db:visit_time(Place, Time),
    (
        Time >= 300, Time < 600 -> triangle_up(300, 600, Time, Factor);
        Time >= 600, Time < 1500 -> Factor is 1;
        Time >= 1500, Time < 3000 -> triangle_down(1500, 3000, Time, Factor);
        Factor is 0
    ).

long_visit_time(Place, Factor) :- 
    db:visit_time(Place, Time),
    (
        Time >= 1500, Time < 3000 -> triangle_up(1500, 3000, Time, Factor);
        Time >= 3000 -> Factor is 1;
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
    map_children_activities_range_to_factor(few, FewMappedFactor),
    map_children_activities_range_to_factor(some, SomeMappedFactor),
    map_children_activities_range_to_factor(many, ManyMappedFactor),
    sharpen(
        [FewFactor, SomeFactor, ManyFactor],
        [FewMappedFactor, SomeMappedFactor, ManyMappedFactor],
        Factor
    ).

few_children_activities_range(Place, Factor) :-
    db:activities_for_children(Place, Quantity),
    (
        Quantity < 5 -> Factor is 1;
        Quantity >= 5, Quantity < 10 -> triangle_down(5, 10, Quantity, Factor);
        Factor is 0
    ).

some_children_activities_range(Place, Factor) :-
    db:activities_for_children(Place, Quantity),
    (
        Quantity >= 5, Quantity < 10 -> triangle_up(5, 10, Quantity, Factor);
        Quantity >= 10, Quantity < 20 -> Factor is 1;
        Quantity >= 20, Quantity < 25 -> triangle_down(20, 25, Quantity, Factor);
        Factor is 0
    ).

many_children_activities_range(Place, Factor) :- 
    db:activities_for_children(Place, Quantity),
    (
        Quantity >= 20, Quantity < 25 -> triangle_up(20, 25, Quantity, Factor);
        Quantity >= 25 -> Factor is 1;
        Factor is 0
    ).

/* Tourists visiting */
tourists_visiting_factor(Place, Factor) :-
    few_tourists_visiting_range(Place, FewFactor),
    some_tourists_visiting_range(Place, SomeFactor),
    many_tourists_visiting_range(Place, ManyFactor),
    map_tourists_visiting_range_to_factor(few, FewMappedFactor),
    map_tourists_visiting_range_to_factor(some, SomeMappedFactor),
    map_tourists_visiting_range_to_factor(many, ManyMappedFactor),
    sharpen(
        [FewFactor, SomeFactor, ManyFactor],
        [FewMappedFactor, SomeMappedFactor, ManyMappedFactor],
        Factor
    ).

few_tourists_visiting_range(Place, Factor) :-
    db:tourists_visiting(Place, Quantity),
    (
        Quantity < 50 -> Factor is 1;
        Quantity >= 50, Quantity < 100 -> triangle_down(50, 100, Quantity, Factor);
        Factor is 0
    ).

some_tourists_visiting_range(Place, Factor) :-
    db:tourists_visiting(Place, Quantity),
    (
        Quantity >= 50, Quantity < 100 -> triangle_up(50, 100, Quantity, Factor);
        Quantity >= 100, Quantity < 500 -> Factor is 1;
        Quantity >= 500, Quantity < 800 -> triangle_down(500, 800, Quantity, Factor);
        Factor is 0
    ).

many_tourists_visiting_range(Place, Factor) :- 
    db:tourists_visiting(Place, Quantity),
    (
        Quantity >= 500, Quantity < 800 -> triangle_up(500, 800, Quantity, Factor);
        Quantity >= 800 -> Factor is 1;
        Factor is 0
    ).

/* Age limit */
age_limit_factor(AgeLimit, Place, Factor) :-
(
    db:age_limit(Place, AgeLimit) -> Factor is 1;
    Factor is 0
).

/* Meals */
has_meal_factor(Place, Factor) :-
(
    db:has_meal(Place) -> Factor is 1;
    Factor is 0
).

/* Rooms to sleep */
has_rooms_to_sleep_factor(Place, Factor) :-
(
    db:has_rooms_to_sleep(Place) -> Factor is 1;
    Factor is 0
).

/* Toilets */
has_toilet_factor(Place, Factor) :-
(
    db:has_toilet(Place) -> Factor is 1;
    Factor is 0
).

/* Parking */
has_parking_factor(Place, Factor) :-
(
    db:has_parking(Place) -> Factor is 1;
    Factor is 0
).

/* Adjustment for disabled */
adjusted_for_disabled_factor(Place, Factor) :-
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