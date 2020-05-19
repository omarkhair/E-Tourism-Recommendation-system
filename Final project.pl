%......................................................FactBase........................................................


offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1),
bus).
offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31),
10, 1), cabin).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding,
50).
customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel,
100).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin,
79).




%......................................................PossibleSubset........................................................
%PossibleSubset Genrates all subsets of a given list with their permutations.


possibleSubset(L,R):-
    subset(L,X),
    permutation(X,R).

subset([],[]).
subset([H|T],[H|N]):-
    subset(T,N).
subset([_|T],N):-
    subset(T,N).
%......................................................choosePreferences...............................................
% choosePreferences Genrates all possible subsets of a given preferences list considering the subsets of the activity list.


choosePreferences(Prefs,ChosenPreferences):-
    member(activity(L),Prefs),
    delete(Prefs,activity(L),P2),
    subset(L,R),
    subset([activity(R)|P2],ChosenPreferences).
choosePreferences(Prefs,ChosenPreferences):-
    \+member(activity(_),Prefs),
    subset(Prefs,ChosenPreferences).

%....................................................preferenceSatisfaction.............................................
% Given the details of a customer, their preferences, and a given offer, the predicate calculates satisfaction by summing all
% the relevance points from the customer's preferred Mean, Activity, and Acommodation.

% Used Helper Predicates: Calculate,satisfactionActivity.
% calculate/4 ( Customer,ChosenPrefs,Acc,Sum).
% This predicates traverses the preferences list to look for target satisfaction structures and behaves accordingly.
% satisfactionActivity/4 (List of activities, Customer, Accumulator(intitially 0), Result).
% A special helper for the activity structures that sums the relevance of all activities.



%offer(destination, activities, cost, validFrom, validTo, period, duration, noOfGuests)
%customer(firstName, lastName, birthDate, status, noOfChildren, job)

preferenceSatisfaction(Offer, Customer, ChosenPrefs, S):-    
   getOffer(ChosenPrefs,Offer),
   calculate(Customer,ChosenPrefs,0,S).


satisfactionActivity([],_,Acc,Acc).
satisfactionActivity([H|T],C,Acc,R):-
    customerPreferredActivity(C,H,S1),
    NewAcc is Acc+S1,
    satisfactionActivity(T,C,NewAcc,R).
    
calculate(_,[],S,S).  
calculate(Customer,[H|T],Acc,S):- 
    H = activity(L),
    satisfactionActivity(L, Customer,0, R),
    Acc2 is Acc+R,
    calculate(Customer,T,Acc2,S).
	
calculate(Customer,[H|T],Acc,S):- 
    H = accommodation(X),
    customerPreferredAccommodation(Customer,X,R),
    Acc2 is Acc+R,
    calculate(Customer,T,Acc2,S).
    
calculate(Customer,[H|T],Acc,S):- 
    H = means(X),
    customerPreferredMean(Customer,X,R),
    Acc2 is Acc+R,
    calculate(Customer,T,Acc2,S).

calculate(Customer,[H|T],Acc,S):- 
     H \= means(_),
     H \= accommodation(_),
     H \= activity(_),
    calculate(Customer,T,Acc,S).
%.....................................................PeriodRelatedPredicates...........................................................
% This predicate (overlapPeriod) checks if two periods are overlapping. 
% Two periods are not overlapping if the first ends before the second starts.
% Used Helper Predicates: after,before.
% after/6 (Year1,Month1,Day1,Year2,Month2,Day2) true if Date1 comes after Date2
% before/6 (Year1,Month1,Day1,Year2,Month2,Day2) true if Date1 comes before Date2


overlapPeriod(period(FY1-FM1-FD1,TY1-TM1-TD1),period(FY2-FM2-FD2,TY2-TM2-TD2)):-
    \+ after(FY2,FM2,FD2,TY1,TM1,TD1),
    \+ after(FY1,FM1,FD1,TY2,TM2,TD2).
after(Y1,_,_,Y2,_,_):-
    Y1>Y2.
after(Y1,M1,_,Y2,M2,_):-
    Y1==Y2,
    M1>M2.
after(Y1,M1,D1,Y2,M2,D2):-
    Y1==Y2,
    M1==M2,
    D1>D2.
before(Y1,_,_,Y2,_,_):-
    Y1<Y2.
before(Y1,M1,_,Y2,M2,_):-
    Y1==Y2,
    M1<M2.
before(Y1,M1,D1,Y2,M2,D2):-
    Y1==Y2,
    M1==M2,
    D1<D2.
%..........................................................getOffer........................................................
% Given a list of preferences, this predicate generate all offers that satisfy the whole list.
% offerMean is used every time to match the offer with the data base since it's not a stand-alone fact.
% The predicate traverses the given list and apply the constraints on the search space accordingly.


% offer(destination, activities, cost, validFrom, validTo, period, duration, noOfGuests)
%preferences::::[dest(dahab), period(2020-04-01, 2020-06-15), means(bus), accommodation(hotel), 
% activity([diving,snorkeling, sightSeeing]), budget(5000)]
%getOffer([dest(dahab), means(bus), accommodation(hotel), activity([diving,snorkeling, sightSeeing]), budget(5000)],X).


getOffer([],_).

getOffer([dest(X)|T],Offer):-
    offerMean(Offer,_),
    Offer=offer(X,_,_,_,_,_,_,_),
    getOffer(T,Offer).
    
getOffer([period(From1,To1)|T],Offer):-
    offerMean(Offer,_),
    Offer=offer(_,_,_,_,_,period(From2,To2),_,_),
    overlapPeriod(period(From1,To1),period(From2,To2)),
    getOffer(T,Offer).
    
getOffer([means(X)|T],Offer):-
    offerMean(Offer,X),
    getOffer(T,Offer).
    
getOffer([accommodation(A)|T],Offer):-
    offerAccommodation(Offer,A),
    getOffer(T,Offer).
    
getOffer([activity(L)|T],Offer):-
   offerMean(Offer,_),
   Offer=offer(_,L2,_,_,_,_,_,_),
    possibleSubset(L2,L),
    getOffer(T,Offer).
    
getOffer([budget(B)|T],Offer):-
    offerMean(Offer,_),
    Offer=offer(_,_,C,_,_,_,_,_),
    C=<B,
    getOffer(T,Offer).
%....................................................recommendOfferForCustomer................................................
% Given a list of preferences, The predicate recommendOfferForCustomer(Prefs, ChosenPrefs, O) chooses from the list Prefs a
% subset ChosenPrefs that could be satisfied by the offer O.


recommendOfferForCustomer(Prefs, ChosenPrefs, O):-
    choosePreferences(Prefs,ChosenPrefs),
    getOffer(ChosenPrefs,O).
%.....................................................recommendOffer.........................................................
% Given a list of Customers and their corresponding preferences, the predicate recommendOffer/4 recommends an Offer
% and chooses the most N satisfied Customers.

% Used Helper Predicates: generateMaxSatList,searchForMaxN,calculateMaxOfOneCust,getMax,findHighest,deleteByIndex,getCorresponding.
% generateMaxSatList/5 (List of Customers , List of thier Prefs , Offer , Acc(intitially []) , List of Max Satisfaction per Customer)
% Given a certain Offer, it generates a a list of Satisfaction points.

% searchForMaxN/5 (List of Customers , List of Max Satisfaction per Customer , N (offer capacity) , Acc(intitially []) , CustomersChosen)
% Given a list of customers and their Satisfaction, this predicate returns the most N satisfied customers by a given offer.

% calculateMaxOfOneCust/4 (Offer ,PreferenceList , Customer , MaxSatisfaction)
% calculates the MaxSatisfaction among all possibleSubsets of PreferenceList

% getMax/5 (List of all chosen prefs ,Acc , Max , Offer, Customer)
% findHighest/2 (List of Max Satisfaction, Index corresponding to Highest Value)
% deleteByIndex/3 (IndexToBeDeleted,List, New List with deleted element) 
% getCorresponding/3 (IndexToBeFetched,List,ElementToBeFetched)



recommendOffer(Customers, PreferenceList, Offer, CustomersChosen):- 
    offerMean(Offer,_),
    generateMaxSatList(Customers,PreferenceList,Offer,[],ListOfMaxSat),
    Offer=offer(_,_,_,_,_,_,_,N),
    searchForMaxN(Customers,ListOfMaxSat,N,[],CustomersChosen).
    %search for max -->search for next max   search(capicity,[customer],[maxSat],[chosen]) 
    
    
    

generateMaxSatList([],[],_,M,N):-
    reverse(M,N).    
generateMaxSatList([CH|CT],[PH|PT],Offer,AccList,M):-
    calculateMaxOfOneCust(Offer,PH,CH,MaxH),
    generateMaxSatList(CT,PT,Offer,[MaxH|AccList],M).

calculateMaxOfOneCust(Offer,Pref,Customer,Max):-
    setof(ChosenPrefs,recommendOfferForCustomer(Pref, ChosenPrefs, Offer),Ls),
    getMax(Ls,0,Max,Offer,Customer).
    
getMax([],Max,Max,_,_).
getMax([H|T],Acc,Max,Offer,Customer):-
	preferenceSatisfaction(Offer,Customer,H,S1),
	S1>=Acc,
	Acc2=S1,
	getMax(T,Acc2,Max,Offer,Customer).
getMax([H|T],Acc,Max,Offer,Customer):-
	preferenceSatisfaction(Offer,Customer,H,S1),
	S1<Acc,
	getMax(T,Acc,Max,Offer,Customer).
   
 


searchForMaxN([_|_],_,0,X,Y):-
    reverse(X,Y).
searchForMaxN([],[],_,X,Y):-
    reverse(X,Y).
searchForMaxN(Customers,ListOfMaxSat,N,Acc,CustomersChosen):-
    N>0,
    Customers=[_|_],
    findHighest(ListOfMaxSat,Index),
    getCorresponding(Index,Customers,ChosenOne),
    deleteByIndex(Index,Customers,NewCustomers),
    deleteByIndex(Index,ListOfMaxSat,NewListOfMaxSat),
    N1 is N-1,
    searchForMaxN(NewCustomers,NewListOfMaxSat,N1,[ChosenOne|Acc],CustomersChosen).

getCorresponding(0,[H|_],H).
getCorresponding(I,[_|T],E):-
    I>0,
    I1 is I-1,
    getCorresponding(I1,T,E).

deleteByIndex(0,[_|T],T).
deleteByIndex(I,[H|T],[H|L]):-
    I>0,
    I1 is I-1,
    deleteByIndex(I1,T,L).


findHighest(L,I):-
    findHelper(L,I,0,-1,-1).
findHelper([],I,_,_,I).
findHelper([H|T],I,CurI,MaxSoFar,_):-
    H>MaxSoFar,
    NewMax=H,
    Next is CurI+1,
    findHelper(T,I,Next,NewMax,CurI).

findHelper([H|T],I,CurI,MaxSoFar,MaxIndex):-
    H=<MaxSoFar,
    Next is CurI+1,
    findHelper(T,I,Next,MaxSoFar,MaxIndex).
	
%..................................Customer's most preferred activity...........................

getAllActivities(L):-
	setof(X,Y^Z^customerPreferredActivity(Y,X,Z),L).

mostPreferredActivity(C,A):-
	getAllActivities(L),
	most_helper(L,C,0,HighestR),
	customerPreferredActivity(C,A,HighestR).
	
most_helper([],_,Acc,Acc).
most_helper([H|T],C,Acc, Ans):-
	customerPreferredActivity(C,H,R),
	R >= Acc, 
	most_helper(T,C,R,Ans).
most_helper([H|T],C,Acc, Ans):-
	customerPreferredActivity(C,H,R),
	R < Acc, 
	most_helper(T,C,Acc,Ans).







