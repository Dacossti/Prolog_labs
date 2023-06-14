% Licensed under the Creative Commons Attribution 4.0 International License (http://creativecommons.org/licenses/by/4.0/).

implement main
    open core, stdio, file

domains
    climate = cold; hot; temperate.

class facts - countriesDB
    country : (integer I1, string Name1, string Cont, real Pop1).
    capital : (integer I2, string Name2, real Pop2).
    is_capital : (integer I2, integer I1).
    climate : (string Country, climate Clim).

class predicates  %Вспомогательные предикаты
    length : (A*) -> integer N.
    sum : (real* List) -> real Sum.
    average : (real* List) -> real Average determ.
    printList : (string* List).

clauses
    length([]) = 0.
    length([_ | T]) = length(T) + 1.

    sum([]) = 0.
    sum([H | T]) = sum(T) + H.

    average(L) = sum(L) / length(L) :-
        length(L) > 0.

    printList([]) :-
        !.

    printList([Head | Tail]) :-
        write(Head),
        printList(Tail).

class predicates  %Основный предикат
    capitalOf : (string Country) nondeterm.
clauses
    capitalOf(Country) :-
        country(I1, Country, _, _),
        is_Capital(I2, I1),
        capital(I2, Name2, _),
        write("The capital of "),
        write(Country),
        write(" is "),
        write(Name2),
        write("."),
        nl.

class predicates  %Основные предикаты
    countryListof : (string Continent) -> string* Country.
    numberofCountriesin : (string Continent) -> integer N.

clauses
    countryListof(Continent) = [ NameCountry || country(_, NameCountry, Continent, _) ].

    numberofCountriesin(Continent) = length(countryListof(Continent)).

class predicates  %Основные предикаты
    capitalListof : (string Continent) -> string* Capital determ.
clauses
    capitalListof(Continent) = Capital :-
        country(I1, _, Continent, _),
        !,
        Capital =
            [ NameCap ||
                is_Capital(I2, I1),
                capital(I2, NameCap, _)
            ].

class predicates  %Основный предикат
    countriesWithclimate : (climate Climate) -> string* Country.
clauses
    countriesWithclimate(Climate) = [ Country || climate(Country, Climate) ].

class predicates  %Основные предикаты + Вспомогательные предикаты
    popListofCountryin : (string Continent) -> real* Country.
    maxList : (real* List) -> real MaxPop nondeterm.
    mostPopulousCountryin : (string Continent) -> string Country nondeterm.
    minList : (real* List) -> real MinPop nondeterm.
    lessPopulousCountryin : (string Continent) -> string Country nondeterm.
    mostPopulousCountry : () -> string Country nondeterm.
    lessPopulousCountry : () -> string Country nondeterm.

clauses
    popListofCountryin(Continent) = [ Pop || country(_, _, Continent, Pop) ].

    maxList([]) = 0.

    maxList([Head | Tail]) = Head :-
        Head > maxList(Tail).

    maxList([Head | Tail]) = maxList(Tail) :-
        Head < maxList(Tail).

    mostPopulousCountryin(Continent) = Country :-
        country(_, Country, Continent, maxList(popListofCountryin(Continent))),
        minList([]) = 0.

    minList([Head | Tail]) = Head :-
        Head < minList(Tail).

    minList([Head | Tail]) = minList(Tail) :-
        Head > minList(Tail).

    lessPopulousCountryin(Continent) = Country :-
        country(_, Country, Continent, minList(popListofCountryin(Continent))).

    mostPopulousCountry() = Country :-
        country(_, Country, _, maxList([ Pop || country(_, _, _, Pop) ])).

    lessPopulousCountry() = Country :-
        country(_, Country, _, minList([ Pop || country(_, _, _, Pop) ])).

class predicates  %Основный предикат
    partialAveragePop : (string Country1, string Country2, string Country3) -> real AveragePop nondeterm.
clauses
    partialAveragePop(Country1, Country2, Country3) = AveragePop :-
        country(_, Country1, _, Pop1),
        country(_, Country2, _, Pop2),
        country(_, Country3, _, Pop3),
        AveragePop = (Pop1 + Pop2 + Pop3) / 3.

class predicates  %Основный предикат
    totalAveragePop : () -> real TotalAveragePop determ.
clauses
    totalAveragePop() = average([ Pop || country(_, _, _, Pop) ]).

class predicates  %Основный предикат
    country_with_more_than : (integer Pop) -> string* CountrymorethanPop determ.
clauses
    country_with_more_than(Pop) = CountrymorethanPop :-
        country(_, Country, _, Pop1),
        !,
        CountrymorethanPop = [ Country || Pop1 > Pop ].

clauses
    run() :-
        consult("../fa.txt", countriesDB),
        !.

    run() :-
        capitalOf("Haiti"),
        !.

    run() :-
        printList(capitalListof("America")),
        !.

    run() :-
        printList(countryListof("Europa")),
        !.

    run() :-
        C = mostPopulousCountryin("America"),
        writef("The most populous country in America is %t", C),
        !.

    run() :-
        C = lessPopulousCountryin("Europa"),
        writef("The less populous country in Europa is %t", C),
        !.

    run() :-
        C = mostPopulousCountry(),
        writef("The most populous country is %t", C),
        !.

    run() :-
        C = lessPopulousCountry(),
        writef("The less populous country is %t", C),
        !.

    run() :-
        AveragePop = partialAveragePop("USA", "Spain", "Japan"),
        writef("The average population for these 3 countries is equal to %", AveragePop),
        !.

    run() :-
        AveragePop = totalAveragePop(),
        writef("The average population for all the countries in our database is equal to %", AveragePop),
        !.

    run() :-
        printList(country_with_more_than(100)),
        !.

    run() :-
        printList(countriesWithclimate(cold)),
        !.

    run().

end implement main

goal
    console::runUtf8(main::run).
