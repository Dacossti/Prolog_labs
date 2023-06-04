/*Country(Id, Name, Continent, Population(million))*/

country(0, "USA", "America", 331.9).
country(1, "Canada", "America", 38.25).
country(2, "China", "Asia", 1455.1).
country(3, "France", "Europe", 67.75).
country(4, "Congo", "Africa", 95.89).
country(5, "Spain", "Europe", 47.42).
country(6, "New_Zealand", "Australia", 5.123).
country(7, "Haiti", "America", 11.45).
country(8, "Madagascar", "Africa", 28.92).
country(9, "Japan", "Asia", 125.7).

/*Capital(Id, Name, Population(million))*/

capital(10, "Tokyo", 13.96).
capital(11, "Antananarivo", 1.275).
capital(12, "Port_au_Prince", 0.987).
capital(13, "Wellington", 0.212).
capital(14, "Paris", 3.223).
capital(15, "Brazzaville", 1.696).
capital(16, "Madrid", 2.161).
capital(17, "Beijing", 21.54).
capital(18, "Ottawa", 0.994).
capital(19, "Washington_DC", 0.712).

/*Is_Capital(Id_capital, Id_country).*/

is_Capital(10, 9).
is_Capital(11, 8).
is_Capital(12, 7).
is_Capital(13, 6).
is_Capital(14, 3).
is_Capital(15, 4).
is_Capital(16, 5).
is_Capital(17, 2).
is_Capital(18, 1).
is_Capital(19, 0).

/*Rule to find the capital of a country*/

capitalOf(X):- 
                    country(I1, X, _, _), 
                    is_Capital(I2, I1), 
                    capital(I2, Name, _), 
                    write("The capital of "), 
                    write(X), 
                    write(" is "), 
                    write(Name), 
                    write("."), 
                    nl.

/*Rules to get a list of all the capital of a continent*/

list_member(X,[X|_]).
list_member(X,[_|TAIL]) :- list_member(X, TAIL).

list_append(A, T, T) :- 
                                list_member(A , T), !.
                                list_append(A, T, [A|T]).
                                                               
capitalListof(X, CapList):- 
                        country(I1, _, X, _), 
                        is_Capital(I2, I1), 
                        capital(I2, Name, _),
                        list_append(Name, CapList , CapList),
                        write("Capital list of "),
                        write(X),
                        write(" includes "),
                        write(Name), 
                        write("."),
                        nl.                
                        
/* ИЛИ
Here, "findall" is a built-in function of prolog to collect a list of the items using a template and bind the list to get a required goal.

capitalListof(X):- 
                        country(I1, _, X, _), 
                        is_Capital(I2, I1), 
                        findall(Cap, capital(I2, Cap, _), CapList). */

                        
/*Rule to check if a country is in a continent*/

in_Continent(X, Continent):- 
                                            country(_, X, Continent, _), 
                                            write("Yes, "), 
                                            write(X), 
                                            write(" is in the continent "), 
                                            write(Continent), 
                                            write("."), 
                                            nl.                                       
in_Continent(X, Continent):- 
                                            country(_, X, Name, _), 
                                            write("No, "), 
                                            write(X), 
                                            write(" is not in the continent "), 
                                            write(Continent), write("."), 
                                            nl.

/*Rules to check if two countries are in the same continent*/

in_Same_continent(X1, X2):- 
                                            country(_, X1, Name, _), 
                                            country(_, X2, Name, _), 
                                            write("Yes, "), 
                                            write(X1), 
                                            write(" and "), 
                                            write(X2), 
                                            write(" are in the same continent ."), 
                                            nl.
                                        
in_Same_continent(X1, X2):- 
                                            country(_, X1, Name1, _), 
                                            country(_, X2, Name2, _), 
                                            write("No, "), 
                                            write(X1), 
                                            write(" and "), 
                                            write(X2), 
                                            write(" are not in the same continent ."), 
                                            nl.

/*Rules to compare two countries using their population*/

comparePop(X1, X2) :- 
                                    country(_, X1, _, P1), 
                                    country(_, X2, _, P2), 
                                    P1 < P2, 
                                    write(X1), 
                                    write(" is less populous than "), 
                                    write(X2), 
                                    nl.
comparePop(X1, X2) :- 
                                    country(_, X1, _, P1), 
                                    country(_, X2, _, P2), 
                                    P1 > P2, 
                                    write(X1), 
                                    write(" is more populous than "), 
                                    write(X2), 
                                    nl.

/*Rules for finding the most populous country or capital for a continent*/

max_of_two(X, Y, X) :- X >= Y.
max_of_two(X, Y, Y) :- X < Y.

list_max_elem([X], X).
list_max_elem([X,Y|Rest], Max) :-
                                                list_max_elem([Y|Rest], MaxRest),
                                                max_of_two(X, MaxRest, Max).



countryPopList(Continent, CountryPopList):-
                                                            country(_, _, Continent, Pop), 
                                                            list_append(Pop, CountryPopList, CountryPopList).
                                                
                                                
most_PopCountry_in(Continent):- 
                                                getCountryPopList(Continent, CountryPopList),
                                                list_max_elem(CountryPopList, Max), 
                                                country(_, Name, Continent, Max), 
                                                write("The most populous country in "),
                                                write(Continent),
                                                write(" is "),
                                                write(Name),
                                                nl.

/* ИЛИ
Here, "findall" is a built-in function of prolog to collect a list of the items using a template and bind the list to get a required goal.

most_PopCountry_in(Continent):- 
                                                findall (P, country(_, _, Continent, P), PopList), 
                                                list_max_elem(PopList, Max), 
                                                country(_, Name, Continent, Max), 
                                                write("The most populous country in "),
                                                write(Continent),
                                                write(" is "),
                                                write(Name),
                                                nl.
*/


getCapitalPopList(Continent, CapPopList):-
                                                            country(I1, _, Continent, _), 
                                                            is_Capital(I2, I1), 
                                                            capital(I2, _, Pop),
                                                            list_append(Pop, CapPopList, CapPopList).


most_PopCapital_in(Continent):- 
                                                getCapitalPopList(Continent, CapPopList),
                                                list_max_elem(CapPopList, Max), 
                                                capital(_, Name, Max), 
                                                write("The most populous capital in "),
                                                write(Continent),
                                                write(" is "),
                                                write(Name),
                                                nl.

/*
most_PopCapital_in(Continent):- 
                                                country(I1, _, X, _), 
                                                is_Capital(I2, I1), 
                                                findall(P, capital(I2, _, P), CapitalPopList),
                                                list_max_elem(CapPopList, Max), 
                                                capital(_, Name, Max),
                                                write("The most populous capital in "),
                                                write(Continent),
                                                write(" is "),
                                                write(Name),
                                                nl.

*/
