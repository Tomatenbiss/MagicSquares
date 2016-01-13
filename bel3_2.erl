-module(bel3_2).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Algorithmus fuer die verteilte Berechnung Magischer Quadrate
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%% Teil 1 - Berechnung magischer Quadrate auf einem Rechner  %%%%

% Berechnet alle moeglichen Zeilen eines Magischen Quadrats
% Aufruf: row(Max, Value) - z.B. row(3,15,lists:seq(1,15))
% Max - Seitengroesse des Quadrats
% Value - Wert der Summe der Zeile
% Elements - Elemente aus denen ausgewaehlt werden soll
-spec row(non_neg_integer(), non_neg_integer(),list(non_neg_integer())) -> list(list(non_neg_integer())).
row(Max, Value, Elements) -> row(Max, Value, Elements,  Max).
row(0, _, _, _) -> [[]];
row(Max, Value, Elements, Ref) -> [X++[Y]||X <- row(Max - 1, Value, Elements, Ref), Y<-Elements--X, valid(Max, Value, X++[Y], Ref)].

valid(Ref, Value, L, Ref) -> case (sum(L) == Value) of
  true -> true;
  _ -> false
end;
valid(_, _, _, _) -> true.

sum(List) -> lists:foldl(fun(X, Sum) -> X + Sum end, 0, List).

% Funktion, die ermittelt, ob sich in zwei Listen doppelte Elemente befinden
% Aufruf duplicate(Liste1,Liste2)
% Liste1 - Erste Liste
% Liste2 - Zweite Liste
contains(_,[]) -> false;
contains(El,[L|_]) when El == L -> true;
contains(El,[_|LS]) -> contains(El,LS).

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
combineRows(Col, Max, Value) -> combineRows(Col, Max, Value, row(Max,Value,lists:seq(1, Max * Max))).
combineRows(0, _, _, _) -> [[]];
combineRows(Col,Max,Value, Elements) -> [X++[Y]||X<-combineRows(Col-1, Max, Value, Elements), Y<-Elements--X, validCR(X++[Y])].

validCR([]) -> true;
validCR([X|XS]) -> case helperValidCR(X, XS) of
  true -> validCR(XS);
  _ -> false
end.

helperValidCR(_, []) -> true;
helperValidCR(El, [L|LS]) -> case duplicate(El, L) of
  true -> false;
  _ -> helperValidCR(El, LS)
end.


% calcSquares berechnet aus einem Teilquadrat alle moeglichen gueltigen Quadrate, die sich bilden lassen
% Aufruf: calcSquares(Part, Max, Value)
% Part - Teilquadrat fuer das die Magischen Quadrate berechnet werden sollen
% Max - Anzahl der Elemente pro Zeile/Spalte
% Value - Wert der Summe einer Zeile
-spec calcSquares(list(non_neg_integer()), non_neg_integer(), non_neg_integer()) -> list(list(non_neg_integer())).
calcSquares(Part, Max, Value) -> Elements = combineRows((Max * Max - length(Part)) div Max, Max, Value, row(Max, Value, lists:seq(1, Max * Max)--Part)),
  [lists:flatten(Part++Res)||Res <- Elements].
%%% combineRows with reduced amount of rows to be combined. Elements


%%% Expects a List of Calced Squares..
evalSquares([], _, _) -> [];
evalSquares([X|XS], Max, Value) -> D = iterCalcedSquares(X, Max), case validSquare(D, Max, Value) of
  true -> [X|evalSquares(XS, Max, Value)];
  _ -> evalSquares(XS, Max, Value)
end.


validSquare(D, Max, Value) -> Vals = lists:map(fun({_, Y}) -> Y end, dict:to_list(D)),
                         N = lists:foldl(fun(X, N) when X == Value -> N + 1; (_, N) -> N end, 0, Vals),
                         case (N == Max + 2) of
                           true ->  true;
                           _ -> false
                         end.


iterCalcedSquares(List, Max) -> iterCalcedSquares(List, 1, Max * Max, Max, createDict(Max)).
iterCalcedSquares([], _, _, _, D) -> D;
iterCalcedSquares([X|XS], C, Len, Max, D) -> Dict = updateCols(C, Max, X, updateMD(C, Max, X, updateSD(C, Max, X, D))),
                                             iterCalcedSquares(XS, C+1, Len, Max, Dict).

updateCols(C, Max, X, D) -> Key = C rem Max,
                         updateDict(Key, D, X).

updateMD(C, Max, X, D) -> case mainDia(C, Max) of
  true -> updateDict(mainDia, D, X);
  _ -> D
end.

updateSD(C, Max, X, D) -> case getSideDia(C, Max) of
  true -> updateDict(sideDia, D, X);
  _ -> D
end.

updateDict(Key, D, X) -> Temp = Temp = dict:fetch(Key, D),
                     Dict = dict:store(Key, Temp + X, D).
createDict(Max) -> dict:store(sideDia, 0, dict:store(mainDia, 0, createDictCols(0, Max ,dict:new()))).

createDictCols(Max, Max, D) -> D;
createDictCols(C, Max, D) -> createDictCols(C+1, Max, dict:store(C, 0, D)).

% Diagnole
getSideDia(C, Max) when (C == Max) -> true;
getSideDia(C, Max) when (C == Max * Max) -> false;
getSideDia(C, Max) when (C > Max) -> case ((C - Max) rem (Max - 1) == 0) of
  true -> true;
  _ -> false
end;
getSideDia(_, _) -> false.

mainDia(C, Max) -> case (C rem (Max + 1) == 1) of
  true -> true;
  _ -> false
end.

% combineSquares ermittelt aus allen Teilquadraten die gueltige Loesung
% Aufruf: combineSquares(Parts, Max, Value)
% Parts - Alle Teilquadrate
% Max - Anzahl der Zeilen
% Value - Wert der Summe einer Zeile
-spec combineSquares(list(list(non_neg_integer())), non_neg_integer(), non_neg_integer(), integer())->list(list((non_neg_integer()))).
combineSquares([],_, _, _) -> [];
combineSquares([X|XS], Max, Value, Num) ->
	Res= calcSquares(X,Max,Value),
	case Res of
		[] -> combineSquares(XS, Max, Value, Num);
		_ ->	io:format("Erg Nummer~p:~p~n",[Num,Res]),Res++combineSquares(XS, Max, Value,Num+length(Res))
	end.

combineSquares(Parts, Max, Value) ->
	lists:flatmap(fun(X)->calcSquares(X,Max,Value) end, Parts).


magicsquare(Max)-> magicsquare(Max, egal).
magicsquare(Max, Mode)->
	statistics(runtime),
	Result= case Mode of
			debug ->  case Max of
					3-> Parts= combineRows(2,3,15, lists:seq(1, 9)), combineSquares(Parts,3,15,0);
					4-> Parts= combineRows(1,4,34, lists:seq(1, 16)), combineSquares(Parts,4,34,0);
					_-> error
				end;
			_ -> case Max of
					3-> Parts= combineRows(2,3,15, lists:seq(1, 9)), combineSquares(Parts,3,15);
					4-> Parts= combineRows(2,4,34, lists:seq(1, 9)), combineSquares(Parts,4,34);
					_-> error
				end
	end,
	{_, Time1} = statistics(runtime),
	U= Time1/ 1000,
	io:format("Anzahl der Quadrate:~p~n",[length(Result)]),
	io:format("Magicsquare Time:~p~n",[U]),
	Result.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%												%	%												%
% 		    Hier beginnt die Verteilung des Algorithmus					%				%												%				%												%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%   Verteilung auf einem Rechner   %%%%%%%%%%%%%%%%%%

% Berechnung Magischer Quadrate
% Funktioniert fuer N=3 und N=4
% Aufruf: distribMS(Max, PCount)
% Max - Anzahl der Reihen/Spalten
% PCount - Anzahl der Prozesse auf die aufgespalten werden soll
% wobei wenn X=3 - die Summe ist wird auf 15 gesetzt
% oder wenn X=4, dann ist die Summe gleich 34
-spec distribMS(non_neg_integer(), non_neg_integer())-> list(list(non_neg_integer())).
distribMS(Max, PCount)->
	statistics(runtime),
	Result=
		case Max of
			3 -> Value=15, PSquare=combineRows(1,Max,Value, lists:seq(1, 9)),
				spawn_at(PCount, node(), PSquare, 3, Value, init_local),
				loop_gather(PCount,[]);
			4 -> Value=34, PSquare=combineRows(2,Max,Value, lists:seq(1, 9)),
				spawn_at(PCount, node(), PSquare, 4, Value, init_local),
				loop_gather(PCount,[]);
			_ ->  [[]]
		end,
	{_, Time1} = statistics(runtime),
	U= Time1/ 1000,
	io:format("Anzahl der Quadrate:~p~n",[length(Result)]),
	io:format("Magicsquare Time:~p~n",[U]),
	Result.

% Spawnt eine festgelegte Anzahl von Prozessen auf einem angegebenen Host
% Aufruf: spawn_at(CCount, Host, Count, Plist, Max, Value)
% CCount - Anzahl der Prozesse, die abgespalten werden sollen
% Host - Host auf dem der Prozess erzeugt werden soll / wird in diesem Teil nicht benoetigt,
% 		 da alles auf dem lokalen Rechner stattfindet
% InitFun - Funktion, die beim Initialisieren des Prozesses aufgerufen werden soll
-spec spawn_at(integer(), atom(), list(list(non_neg_integer())), non_neg_integer(), non_neg_integer(), atom()) -> ok.
spawn_at(CCount, Host, PList, Max, Value, InitFun)-> toBeDefined.

% Methode, die bei Abspaltung des Prozesses aufgerufen wird
% hat die/den Parameter [Nr, SPid, PList, Max, Value, Host]
% Die Methode berechnet fuer eine Menge an Teilquadraten alle Loesungen und
% sendet diese an den erzeugenden Prozess.
% Nr - Nummer des Prozesses (nur fuer debug-Ausgaben auf der Konsole)
% SPid - Prozessnummer des erzeugenden Prozesses - fuer das Senden des Ergebnisses
% PList - Teilliste, fuer die ein Prozess die magischen Quadrate berechnen soll
% Max - Anzahl der Spalten/Zeilen
% Value - Wert der Summe der Zeile
% Host - kann hier vernachlaessigt werden
init_local(Nr, SPid, PList, Max, Value,_)->
	distrib_calc_squares(Nr, SPid, PList, Max, Value).

-spec distrib_calc_squares(non_neg_integer(), pid(), list(list(non_neg_integer())), non_neg_integer(), non_neg_integer()) -> ok.
distrib_calc_squares(Nr, SPid, PList, Max, Value)-> toBeDefined.

% Methode sammelt alle Ergebnisse ein
% Wird von der Methode magicsquare aufgerufen
% Aufruf (CCount, Result)
% CCount - Anzahl der Prozesse, die gestartet wurden (entspricht der Anzahl der
%		   zu erwartenden Ergebnisse
% Result - Aktuell bereitstehendes Ergebnis

-spec loop_gather(non_neg_integer(), list(list(non_neg_integer())))-> list(list(non_neg_integer())).
loop_gather(CCount,Result)-> toBeDefined.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%								   									%%
%%		Verteilung auf mehrere Rechner			   					%%
%%								   									%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Codieren der Hostnamen mit der Anzahl von Prozessen, die sie ausfuehren sollen
hosts()->[{'tiger@hadoop03',48},{'scorpion@hadoop06',48}].

% Berechnung der Anzahl der Prozesse insgesamt
% Soll fuer die Aufteilung der Quadrate verwendet werden
c_count()-> lists:sum([Count||{_,Count}<-hosts()]).


% Berechnung Magischer Quadrate
% Funktioniert fuer N=3 und N=4
% Aufruf: distribMS(Max, PCount)
% Max - Anzahl der Reihen/Spalten
% PCount - Anzahl der Prozesse auf die aufgespalten werden soll
% wobei wenn X=3 - die Summe ist wird auf 15 gesetzt
% oder wenn X=4, dann ist die Summe gleich 34

megaDistribMS(Max)->

	% Ausschalten des Error-Loggings auf der Konsole
	error_logger:tty(false),
	register(host_monitor,spawn(fun()->init_host_monitor(hosts()) end)),
	statistics(runtime),
	Result=
		case Max of
			3 -> Value=15, PSquare=combineRows(2,Max,Value),
				while(c_count(), hosts(), PSquare, 3, 15),
%				spawn_at(4, node(), PSquare, 3, Value),
				loop_gather(c_count(),[]);
			4 -> Value=34, PSquare=combineRows(2,Max,Value),
				while(c_count(), hosts(), PSquare, 4, 34),
%				spawn_at(4, node(), PSquare, 4, Value),
				loop_gather(c_count(),[]);
			_ ->  [[]]
		end,
	{_, Time1} = statistics(runtime),
	U= Time1/ 1000,
	io:format("Anzahl der Quadrate:~p~n",[length(Result)]),
	io:format("Magicsquare Time:~p~n",[U]),
	host_monitor!stop,
	Result.

% Schleife fuer das spawnen der Prozesse auf mehreren Rechnern
% Benutzt die Methode spawn_at(...)
% Aufruf: while (CCount, Hosts, PList, Max, Value)
% CCount - Anzahl der Prozesse die gespawnt werden sollen
% Hosts - Hostliste der Form { VM-Name, Anzahl der Prozesse}
% PList - Liste der Teilquadrate
% Max - Anzahl der Elemente, die berechnet werden sollen
% Value - Wert der Summe der Zeile
-spec while(non_neg_integer(), list({atom(),non_neg_integer()}), list(list(non_neg_integer())), non_neg_integer(),non_neg_integer())->ok.
while (CCount, HostCountL, PList, Max, Value) -> toBeDefined.

% Supervisor-Prozess, der die Ausfuehrung der Berechnungen ueberwacht
% Spawnt die Berechnungsprozesse auf den Nodes des Erlang-Clusters und behandelt die Fehlerfaelle
% Nr - Nummer des Prozesses (nur zur besseren Identifikation)
% SPid - Prozessnummer des erzeugenden Prozesses
% PList - Teilliste, fuer die ein Prozess die magischen Quadrate berechnen soll
% Max - Anzahl der Spalten/Zeilen
% Value - Wert der Summe der Zeile
% Try - Anzahl der noch ausstehenden Versuche

init_global(Nr, SPid, PList, Max, Value, Host)->
	init_global(Nr, SPid, PList, Max, Value, Host,3).

-spec init_global(non_neg_integer(), pid(), list(list(non_neg_integer())), non_neg_integer(), non_neg_integer(),
	atom(), non_neg_integer()) -> ok.
init_global(Nr, SPid, PList, Max, Value, Host, Try)-> toBeDefined.

% Monitoring-Prozess fuer die Ueberwachung der zur Verfuegung stehenden Cluster-Nodes
% Er wird von der Hauptmethode megaDistribMS gestartet,
% Der Prozess kann ueber das Atom host_monitor angesprochen werden.
% Er beinhaltet die folgenden Operationen:
%  getnode - Ermittlung eines verfuegbaren Nodes
%  addnode - Hinzunahme eines Nodes
%  gethosts - Ermittlung aller verfuegbaren Hosts
%  deletenode - Loeschen eines Nodes

init_host_monitor(MonitorList) -> ML= lists:map(fun({Host,_})->Host end, MonitorList),
	lists:foreach(fun(Host)->erlang:monitor_node(Host, true) end, ML),
	monitorHosts(ML).

monitorHosts([])-> erlang:error(no_hosts_available);
monitorHosts(HostList)->
	receive
		{nodedown, NodeName}-> io:format("Host ~p is down!~n",[NodeName]),
			monitorHosts(lists:delete(NodeName, HostList));
		{getnode, From}-> io:format("Host ~p is requested!~n",[hd(HostList)]),
			From!{newhost, hd(HostList)}, monitorHosts(tl(HostList)++[hd(HostList)]);
		{addnode, NodeName}-> io:format("Host ~p is added!~n",[NodeName]),
			monitor_node(NodeName, true),
			monitorHosts([NodeName|HostList]);
		{gethosts, From} -> From!{hostlist, HostList}, monitorHosts(HostList);
		{deletenode, NodeName}-> io:format("Host ~p will be deleted!~n",[NodeName]),
			monitorHosts(lists:delete(NodeName, HostList));
		stop -> ok
	end.


main() -> D = createDict(3),
          evalSquares(calcSquares([4,9,2], 3, 15), 3, 15).
