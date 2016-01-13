-module(test).
-compile(export_all).

-spec row(non_neg_integer(), non_neg_integer(),list(non_neg_integer())) -> list(list(non_neg_integer())).
row(Max, Value, Elements) -> row(Max, Value, Elements,  Max).
row(0, _, _, _) -> [[]];
%row(Max, Value, Elements) -> [X++[Y]||X <- row(Max - 1, Value, Elements),
%                Y <- Elements--X, valid(Max, Value, Elements--X--[Y])].
row(Max, Value, Elements, Ref) -> [X++[Y]||X <- row(Max - 1, Value, Elements, Ref), Y<-Elements--X, valid(Max, Value, X++[Y], Ref)].



valid(Ref, Value, L, Ref) -> case (sum(L) == Value) of
  true -> true;
  _ -> false
end;
valid(_, _, _, _) -> true.

%%% H ist the reference value for initial comparison, std should be 0
highest(H, []) -> H;
highest(H, [X|XS]) -> case (X < H) of
  true -> highest(H, XS);
  _ -> highest(X, XS)
end.

%%% Returns a List with the n highest Elements in a List
nhighest(0, _, _) -> [];
nhighest(N, Elements, Acc) -> H = highest(0, Elements), [H|nhighest(N - 1, Elements--[H], Acc)].



print (P)->io:write(P),io:fwrite("~n"),true.

sum(List) -> lists:foldl(fun(X, Sum) -> X + Sum end, 0, List).

% Funktion, die ermittelt, ob sich in zwei Listen doppelte Elemente befinden
% Aufruf duplicate(Liste1,Liste2)
% Liste1 - Erste Liste
% Liste2 - Zweite Liste
contains(El,[]) -> false;
contains(El,[L|LS]) when El == L -> true;
contains(El,[L|LS]) -> contains(El,LS).

-spec duplicate(list(non_neg_integer()),list(non_neg_integer())) -> true | false.
duplicate(L1,L2) when (L1 == []) or (L2 == []) -> false;
duplicate([L|LS],L2) ->
	case contains(L,L2) of
		true -> true;
		_ -> duplicate(LS,L2)
	end.

% combineRows setzt eine beliebige Anzahl von Reihen, die vorab berechnet werden, zusammen
% Dabei wird ueberprueft, ob sich doppelte Elemente innerhalb der Reihen befinden.
% Aufruf: combineRows (Col, Max, Value)
% Col - Anzahl der Reihen, die berechnet werden sollen
% Max - Anzahl der Elemente pro Zeile
% Value - Wert der Summe der Zeile
% Elems - Elemente aus denen gewaehlt werden soll

-spec combineRows(non_neg_integer(), non_neg_integer(), non_neg_integer(), list(non_neg_integer()))->list(list(non_neg_integer())).
combineRows(0, _, _, _) -> [[]];
combineRows(Col,Max,Value, Elements) -> [X++[Y]||X<-combineRows(Col-1, Max, Value, Elements), Y<-Elements--X, validCR(X++[Y])].

validCR([]) -> true;
validCR([X|XS]) -> case helperValidCR(X, XS) of
  true -> validCR(XS);
  _ -> false
end.

helperValidCR(El, []) -> true;
helperValidCR(El, [L|LS]) -> case duplicate(El, L) of
  true -> false;
  _ -> helperValidCR(El, LS)
end.

-spec calcSquares(list(non_neg_integer()), non_neg_integer(), non_neg_integer()) -> list(list(non_neg_integer())).
calcSquares(Part, Max, Value) -> Elements = combineRows((Max * Max - length(Part)) div Max, Max, Value, row(Max, Value, lists:seq(1, Max * Max)--Part)),
  [lists:flatten(Part++Res)||Res <- Elements].


%%% combineRows with reduced amount of rows to be combined. Elements
%%% Number of Rows to generate = (Max * Max - length(Part)) div Max
%%% Elements to make the Rows with: lists:seq(1, Max * Max) -- Part

divList(List, N) -> lists:map(fun({_,Y}) -> Y end, dict:to_list(divList(List, N, dict:new(), 0))).
divList([],_,D,_) -> D;
divList([X|XS], N, D, C) -> divList(XS, N, dict:append(C rem N, X, D), C+1).
