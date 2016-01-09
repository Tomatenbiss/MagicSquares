-module(test).
-compile(export_all).

-spec row(non_neg_integer(), non_neg_integer(),list(non_neg_integer())) -> list(list(non_neg_integer())).
row(Max, Value, Elements) -> row(Max, Value, Elements,  Max).
row(0, _, _, _) -> [[]];
%row(Max, Value, Elements) -> [X++[Y]||X <- row(Max - 1, Value, Elements),
%                Y <- Elements--X, valid(Max, Value, Elements--X--[Y])].
row(Max, Value, Elements, Ref) -> print(Ref), [X++[Y]||X <- row(Max - 1, Value, Elements, Ref), Y<-Elements--X, valid(Max, Value, X++[Y], Ref)].



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
