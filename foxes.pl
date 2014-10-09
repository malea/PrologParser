% Prog lang HW 1 Problem 1
% FOXES AND HENS TRYING TO CROSS A LAKE IN A BOAT
% By: Malea Grubb

% The list, Config = [RHens, RFoxes, RBoat] , is going to be used to represent
% the current status of the right side of the river. For instance, RHens can be
% 0 to 3, RFoxes can be 0 to 3, and RBoat is either 0 or 1 (0, when the boat is
% on the left side, and 1 when the boat is on the right side of the river). We
% will use this Config to do a series of checks on the current status of where
% the hens/foxes/boat are in order to find solutions to the puzzle. All Foxes
% and Hens START on the left side of the river and the puzzle is considered
% done when all Foxes, all Hens, and the Boat is on the right side of the
% river. 

valid(Config) :-
    Config = [RHens, RFoxes, RBoat],
    between(0,3, RHens),
    between(0,3, RFoxes),
    between(0,1, RBoat). 

safe(Config) :-
    Config = [RHens, RFoxes, _RBoat],
    (RHens == 0; RHens >= RFoxes),
    (RHens == 3; (3 - RHens) >= (3 - RFoxes)),!.
    
adjacent(Config,NewConfig) :-
    Config = [RHens, RFoxes, RBoat],
    NewConfig = [NewRHens, NewRFoxes, NewRBoat],
    (
        (RBoat = 0, NewRBoat is 1,
            (
                (NewRHens is RHens + 1, NewRFoxes is RFoxes);
                (NewRHens is RHens + 1, NewRFoxes is RFoxes + 1);
                (NewRHens is RHens + 2, NewRFoxes is RFoxes);
                (NewRHens is RHens, NewRFoxes is RFoxes + 1);
                (NewRHens is RHens, NewRFoxes is RFoxes + 2)
            )
        );
        (RBoat = 1, NewRBoat is 0,
            (
                (NewRHens is RHens - 1, NewRFoxes is RFoxes);
                (NewRHens is RHens - 1, NewRFoxes is RFoxes - 1);
                (NewRHens is RHens - 2, NewRFoxes is RFoxes);
                (NewRHens is RHens, NewRFoxes is RFoxes - 1);
                (NewRHens is RHens, NewRFoxes is RFoxes - 2)
            )
        )
    ),
    valid(NewConfig),
    safe(NewConfig).

%Follows(Start, Rest,Visited).
%Starting at Start, find a valid list of configurations where each is a
%valid next step, ending at the Done state, [3,3,1]. It is essential
%to keep track of visited Configs, otherwise stack overflow! (infinite loop)
follows([3,3,1], [], _).
follows(Start, [Next | Rest], Visited) :-
    adjacent(Start, Next),
    \+ member(Next, Visited),
    follows(Next, Rest, [Next | Visited]).

solve(P) :-
    P = [ [0,0,0] | Rest ],
    follows([0,0,0], Rest, [ [0,0,0] ]).
