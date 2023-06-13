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

class predicates
    printCountries : () multi.
clauses
    printCountries() :-
        country(I1, Name1, _, _),
        is_Capital(I2, I1),
        capital(I2, Name2, _),
        write(Name1, ":\t", Name2),
        nl.
    printCountries() :-
        write("All the countries are shown above\n").

class predicates
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

class predicates
    capitalListof : (string Continent) nondeterm.
clauses
    capitalListof(Continent) :-
        country(I1, _, Continent, _),
        is_Capital(I2, I1),
        capital(I2, Name2, _),
        write("Capital list of "),
        write(Continent),
        write(" includes "),
        write(Name2),
        write("."),
        nl.

class predicates
    in_Continent : (string Country, string Continent) multi.
clauses
    in_Continent(Country, Continent) :-
        country(_, Country, Continent, _),
        write("Yes, "),
        write(Country),
        write(" is in the continent "),
        write(Continent),
        write("."),
        nl.

    in_Continent(Country, Continent) :-
        write("No, "),
        write(Country),
        write(" is not in the continent "),
        write(Continent),
        write("."),
        nl.

class predicates
    in_Same_continent : (string Country1, string Country2) multi.
clauses
    in_Same_continent(Country1, Country2) :-
        country(_, Country1, Name1, _),
        country(_, Country2, Name1, _),
        write("Yes, "),
        write(Country1),
        write(" and "),
        write(Country2),
        write(" are in the same continent ."),
        nl.

    in_Same_continent(Country1, Country2) :-
        write("No, "),
        write(Country1),
        write(" and "),
        write(Country2),
        write(" are not in the same continent ."),
        nl.

class predicates
    comparePop : (string Country1, string Country2) nondeterm.
clauses
    comparePop(Country1, Country2) :-
        country(_, Country1, _, P1),
        country(_, Country2, _, P2),
        P1 < P2,
        write(Country1),
        write(" is less populous than "),
        write(Country2),
        nl.

    comparePop(Country1, Country2) :-
        country(_, Country1, _, P1),
        country(_, Country2, _, P2),
        P1 > P2,
        writef("%t is more populous than %t", [Country1, Country2]),
        nl.

class predicates
    countryPopList : (string Continent) nondeterm.
clauses
    countryPopList(Continent) :-
        country(_, _, Continent, Pop1),
        write(Pop1),
        nl.

class predicates
    greatestCountryPop_in : (string Continent, real Pop) nondeterm.
clauses
    greatestCountryPop_in(_, 0) :-
        !.

    greatestCountryPop_in(Continent, Pop) :-
        country(_, _, Continent, Pop1),
        Pop1 > Pop,
        Pop = Pop1,
        greatestCountryPop_in(Continent, Pop),
        country(_, C, Continent, Pop),
        writef("The most populous country in %t is %t", [Continent, C]).

class predicates
    averagePop : (string Country1, string Country2, string Country3) nondeterm.
clauses
    averagePop(Country1, Country2, Country3) :-
        country(_, Country1, _, Pop1),
        country(_, Country2, _, Pop2),
        country(_, Country3, _, Pop3),
        writef("The average population for these 3 countries is equal to %", (Pop1 + Pop2 + Pop3) / 3).

class predicates
    country_with_more_than : (integer Pop) nondeterm.
clauses
    country_with_more_than(Pop) :-
        country(_, Country, _, Pop1),
        Pop1 > Pop,
        write(Country).

clauses
    run() :-
        consult("../fa.txt", countriesDB),
        !.

    run() :-
        capitalOf("Haiti"),
        !.

    run() :-
        capitalListof("America"),
        !.

    run() :-
        in_Continent("Haiti", "Asia"),
        !.

    run() :-
        in_Same_continent("USA", "China"),
        !.

    run() :-
        comparePop("USA", "China"),
        !.

    run() :-
        countryPopList("Europa"),
        !.

    run() :-
        greatestCountryPop_in("America", 0),
        !.

    run() :-
        averagePop("USA", "Spain", "Japan"),
        !.

    run() :-
        country_with_more_than(100),
        !.

    run() :-
        printCountries(),
        !.
    run() :-
        climate("USA", Clim),
        !.

    run().

end implement main

goal
    console::runUtf8(main::run).
