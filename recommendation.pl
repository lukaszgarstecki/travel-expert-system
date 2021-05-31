:- module(recommendation, [recommend/2]).

:- use_module(reasoning).
:- use_module(variables).

recommend(Answers, FactorWithPlacePairsResult) :-
    findall(
        Factor-Place,
        calculate_place_recommendation_factor(Answers, Place, Factor),
        FactorWithPlacePairs),
    keysort(FactorWithPlacePairs, SortedFactorWithPlacePairs),
    reverse(SortedFactorWithPlacePairs, FactorWithPlacePairsResult).

calculate_place_recommendation_factor([
    Price, VisitTime, Climate, ChildrenActivities, TouristsVisiting,
    AgeLimit, Meals, Rooms, Toilets, Parking, AdjustmentForDisabled],
    Place, Factor
 ) :-
    get_price_factor(Price, Place, PriceFactor),
    get_visit_time_factor(VisitTime, Place, VisitTimeFactor),
    get_climate_factor(Climate, Place, ClimateFactor),
    get_children_activities_factor(ChildrenActivities, Place, ChildrenActivitiesFactor),
    get_tourists_visiting_factor(TouristsVisiting, Place, TouristsVisitingFactor),
    get_age_limit_factor(AgeLimit, Place, AgeLimitFactor),
    get_meals_factor(Place, MealsFactor),
    get_rooms_factor(Place, RoomsFactor),
    get_toilets_factor(Place, ToiletsFactor),
    get_parking_factor(Place, ParkingFactor),
    get_adjusted_for_disabled_factor(Place, AdjustmentFactor),
    RecommandationFactors = [
        PriceFactor,
        VisitTimeFactor,
        ClimateFactor,
        ChildrenActivitiesFactor,
        TouristsVisitingFactor,
        AgeLimitFactor,
        MealsFactor,
        RoomsFactor,
        ToiletsFactor,
        ParkingFactor,
        AdjustmentFactor
    ],
    sum_list_elements(RecommandationFactors, RecommandationFactorsSum),
    Factor is RecommandationFactorsSum / 11,
    true.

sum_list_elements([], 0).
sum_list_elements([ListHeader|ListTailElements], Sum) :-
    sum_list_elements(ListTailElements, TailElementsSum),
    Sum is ListHeader + TailElementsSum.

get_price_factor(Price, Place, Factor) :-
    map_price_to_price_factor(Price, TargetFactor),
    price_factor(Place, PriceFactor),
    Factor is 1 - abs(TargetFactor - PriceFactor).

get_visit_time_factor(VisitTime, Place, Factor) :-
    map_visit_time_to_factor(VisitTime, TargetFactor),
    visit_time_factor(Place, VisitTimeFactor),
    Factor is 1 - abs(TargetFactor - VisitTimeFactor).

get_climate_factor(Climate, Place, Factor) :-
    climate_factor(Climate, Place, Factor).

get_children_activities_factor(ChildrenActivities, Place, Factor) :-
    map_children_activities_range_to_factor(ChildrenActivities, TargetFactor),
    children_activities_factor(Place, ChildrenActivitiesFactor),
    Factor is 1 - abs(TargetFactor - ChildrenActivitiesFactor).

get_tourists_visiting_factor(TouristsVisiting, Place, Factor) :-
    map_tourists_visiting_range_to_factor(TouristsVisiting, TargetFactor),
    tourists_visiting_factor(Place, TouristsVisitingFactor),
    Factor is 1 - abs(TargetFactor - TouristsVisitingFactor).

get_age_limit_factor(AgeLimit, Place, Factor) :-
    age_limit_factor(AgeLimit, Place, Factor).

get_meals_factor(Place, Factor) :-
    has_meal_factor(Place, Factor).

get_rooms_factor(Place, Factor) :-
    has_rooms_to_sleep_factor(Place, Factor).

get_toilets_factor(Place, Factor) :-
    has_toilet_factor(Place, Factor).

get_parking_factor(Place, Factor) :-
    has_parking_factor(Place, Factor).

get_adjusted_for_disabled_factor(Place, Factor) :-
    adjusted_for_disabled_factor(Place, Factor).