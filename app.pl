:- module(app, [main/0]).

:- use_module(user_interface).
:- use_module(expert_interface).
:- use_module(recommendation).
:- use_module(variables).


main :-
    format('~nPlease choose type of the program~n'),
    print_variables([user, expert]),
    read(Opt),
    (
        Opt == user -> user_menu;
        Opt == expert -> expert_menu
    ).

user_menu :-
    format('This is travel place recommender program.~n'),
    format('Answer questions to get place that suits your travel needs.~n'),
    read_answers(Answers),
    format('Our recommendations [0-100]:~n'),
    show_recommendations(Answers).

show_recommendations(Answers) :-
    recommend(Answers, FactorWithPlacePairs),
    print_results(FactorWithPlacePairs).

print_results([]) :-
    format('~n').

print_results([FactorWithPlacePairsHead|FactorWithPlacePairsTail]) :-
    get_key_and_value(FactorWithPlacePairsHead, Factor, Place),
    FactorPercentage is Factor * 100,
    format('~2f% ~w ~n', [FactorPercentage, Place]),
    print_results(FactorWithPlacePairsTail).

get_key_and_value(KeyValuePair, Key, Value) :-
    pairs_keys_values([KeyValuePair], [Key], [Value]).
