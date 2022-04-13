% Author:   Tuohuang Li <tuohuangl@student.unimelb.edu.au>
% Purpose:  Select the best set of Hand cards when 
%           playing the Cribbage game.
%The Cribbage game is an old card game from the 17th century.  
%There are a series of rules that can obtain points from,
%including "add to 15", "Pairs", "Runs", "Flushes" and 
%"One of his nob". 
%In the beginning, the dealer will deal 5 or 6 cards to
%the players depending on how many players are playing together.
%The player must discard 1 or 2 cards and keep 4 cards in
%hand and when the game starts, the player must get as 
%many points as they can. 
%Thus, the aim of this program is to simulate this situation
%and calculate the possible set of hand cards which can yield
%the maximum hand value.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               facts about cards
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%1. rank/suit representation
%The predicate member/2 used to ensure 
%that the rank/suit must be selected
%from the list provided.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rank(R):- 
    member(R, [ace,2,3,4,5,6,7,8,9,10,jack,queen,king]).

suit(S):- 
    member(S, [clubs, diamonds, hearts, spades]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%2. Cards representation (card term)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

card(card(R,S)):-
    (rank(R),suit(S)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%3. Create the deck with total 52 cards use this bult-in predicate: 
%findall(+card(R,S),+(rank(R),suit(S)),-Deck).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

the_deck(Deck):-
    findall(card(R,S),(rank(R),suit(S)),Deck).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%4.  Find out the sequence
%The aim of this part is to represent the following special 
%type as a numeric value:
%ace = 1; jack = 11; queen = 12; king = 13.
%For ranks 2-10, just use their face value
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%get facevalue for each card
facevalue(card(ace,_), 1).
facevalue(card(jack,_), 11).
facevalue(card(queen,_), 12).
facevalue(card(king,_), 13).
facevalue(card(R,_), R):-
    integer(R).

%get point for each card
getpoint(card(ace,_), 1).
getpoint(card(jack,_), 10).
getpoint(card(queen,_), 10).
getpoint(card(king,_), 10).
getpoint(card(R,_), R):-
    integer(R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%helper predicate
%to find all possible combination of all elements 
%in a list
%combs(+List1, -List2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

combs([],[]).
combs([E|L],[E|Co]):-
    combs(L, Co).
combs([_|L],Co):- 
    combs(L, Co).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%15s
%find all possible combinations but without empty list from input list.
%fifteens(+ValueOnly, -Points)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fifteens(ValueOnly, Points):-  
    findall(Comb, combs(ValueOnly, Comb), ListOfCombinations),
    maplist(sum_list, ListOfCombinations, ListofSums),   
    findall(I, nth0(I, ListofSums, 15), List),
    length(List, N),
    Points is N*2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Pairs - find different pairs can get from the
%combination of Hand and Startcard.
%[x,x] = 2 points
%[x,x,x] = 6 points
%[x,x,x,x] = 12 points
%1. Find all sublist
%2. Find all sublists length grater than 2 with same elements using list_to_set/2.
%4. calculate pairs for each list via length
%5. sum up: Points is calculated by number of pairs * 2
%pairs(+Rankslist, -Point).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pairs(Rankslist, Point):-
    findall(Sub, (combs(Rankslist, Sub), length(Sub,Len), Len =:= 2, 
                 list_to_set(Sub, [_])), Allsubs),   
    length(Allsubs, N),
    Point is N*2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%helper predicate
%isIncrement/1
%check if the element in list is increment by 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isIncrement([_]).
isIncrement([A,B|T]):-
    B is A+1,
    isIncrement([B|T]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%helper predicate
%isNotEmpty/1: check if the list is []
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

isEmpty(List):-
    length(List, Len),
    Len =:= 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%runs is to calculate the points can get if Hand and
%start card forms a runs greater than 3.
%take the longest run
%the predicate defined below aims to find all combinations
%and filtered those combinations with findall/3 to find
%desired answer.
%runs(+Cardlist, -Points).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

runs(Rankslist, Points):-
    msort(Rankslist, Sortedlist), 
    findall(Combs, (combs(Sortedlist, Combs), isIncrement(Combs), 
                   length(Combs,Len), Len >= 3), Allruns),
    (
        isEmpty(Allruns)
    ->  Points = 0
    ;   maplist(length, Allruns, Lenlist),
        max_list(Lenlist,Max),         
        findall(Max, member(Max, Lenlist), Maxlenslist),
        sum_list(Maxlenslist, Points)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Flushes
%Flushes have 2 different situations:
%   - Hand cards are of the same suit.
%   - Hand cards & startcard are of the same suit.
%flshes(+[Hand],+Startcard,-Value)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flushes([card(_,S), card(_,S), card(_,S), card(_,S)], card(_,S), 5).
flushes([card(_,S), card(_,S), card(_,S), card(_,S)], card(_,S0), 4):-
    S \= S0.
flushes(Hand,_,0):-
    member(card(_,S1), Hand),
    member(card(_,S2), Hand),
    S1 \= S2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%One of his nob
%hand contains jack(j) and j has same suit with start cards - 1 point
%nob(+Hand, +Startcard, -Vn).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nob(Hand, card(_,S), Points):-
    (
        member(card(jack,S), Hand)
    ->  Points = 1
    ;   Points = 0
    
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%      
%main predicate 1: hand_value(+Hand, +Startcard, -Value) 
%Value is an integer Value (an integer) is the total cribbage point 
%value of Hand when Startcard is the start card       
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hand_value(Hand, Startcard, Value):-
    Cardlist = [Startcard|Hand],
    maplist(facevalue, Cardlist, Rankslist),
    maplist(getpoint, Cardlist, ValueOnly),
    fifteens(ValueOnly, V15),
    pairs(Rankslist, Vp),
    runs(Rankslist, Vr),
    flushes(Hand, Startcard, Vf),
    nob(Hand, Startcard, Vn),
    sumlist([V15, Vp, Vr, Vf, Vn], Value).
                              

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%main predicate 2: select_hand(+Cards, -Hand, -Cribcards)
%Cards - 5/6 ramdon cards from dealer
%Hand - selected 4 cards with the highest expected value.
%Cribcards - a list of the cards not kept
% 1. select 4 from 6: get all possible combinations
% 2. send each 'Hand' card combination to hand_value
% 3. each collection of Hand cards, need to combine with a Start card
% 4. collect all hand_values
% 5. get the average value of the hand over all possible start cards
% 6. this average value is the final result we wanted:
%    the expected value of the hand
% 7. To understand which set of 'Hand' give us the highest Value, 
%    we need to use nth 0/3 to find out the index of the highest
%    average score in average score list.
%    Using this Index, we can find the best set of Hand. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

select_hand(Cards, Hand, Cribcards):-
    %Find 15 possible set of 'Hand'
    findall(Handcard, (length(Handcard, 4), combs(Cards, Handcard)), 
            Handcardscombos), 
    %Each run, take one Hand to match each Startcatrd of all 46/47 stardcards, 
    %and return average. 
    %-> get 15 average value
    maplist(expected_handvalue_helper(Cards), Handcardscombos, Averagelist),  
    max_list(Averagelist, HighestAVG),
    (
        nth0(Inx, Averagelist, HighestAVG)
    ->  nth0(Inx, Handcardscombos, Hand)
    ;   false
    ),
    subtract(Cards, Hand, Cribcards).
    %!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%helper predicate
%expected_handvalue_helper(+Cards, +Hand, -AverageValue)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
expected_handvalue_helper(Cards, Hand, AverageValue):-
    the_deck(Deck),
    %get 46/47 kinds of Stard card
    subtract(Deck,Cards,RestAsStartCards), 
    %get 46/47 Values from Hand combine with different Startcards; 
    %Values is a list with length of 15 contains sublists with length of 46/47. 
    maplist(hand_value(Hand), RestAsStartCards, Values),
    getAverage(Values, AverageValue).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%helper predicate
%getAverage(+List, -Ave)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getAverage(List, Ave) :-
    sum_list(List, Sum),
    length(List, Len),
    (  
        Len > 0 
    ->  Ave is Sum / Len   
    ;   Ave is 0
      
    ).
