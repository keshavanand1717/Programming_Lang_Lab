% Check if a number is prime
is_prime(2).
is_prime(3).
is_prime(P) :-
    integer(P),
    P > 3,
    P mod 2 =\= 0,  % Ensure P is odd
    \+ has_factor(P, 3).

% This predicate checks whether N has a factor Factor
has_factor(N, Factor) :-
    N mod Factor =:= 0.
has_factor(N, Factor) :-
    Factor * Factor < N,
    NextFactor is Factor + 2,
    has_factor(N, NextFactor).

% Predicates to handle invalid input
goldbach(Even, _) :-
    Even =< 2,
    write('Invalid Input. Please enter an even number greater than 2'), nl.

goldbach(Even, _) :-
    Even mod 2 =\= 0,  % Ensure Even is even
    write('Invalid Input. Please enter an even number greater than 2'), nl.

% Goldbachs conjecture predicate
goldbach(Even, [P1, P2]) :-
    Even > 2,
    Even mod 2 =:= 0,  % Ensure Even is even
    between(2, Even, P1),
    is_prime(P1),
    P2 is Even - P1,
    is_prime(P2),
    write('Primes are '), write(P1), write(' '), write(P2), nl.
    

