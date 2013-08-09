%% Author: Alberto Lorente Leal, 
%% albll@kth.se
%% a.lorenteleal@gmail.com
%% Created: 24/09/2011
%% Description: Based on the code of gsm1, we add
%% failure detection of the leaders. We reelect a new one.
-module(gms2).

-export([start/1, start/2]).

-define(arghh,100).

start(Id) ->
	Rnd =random:uniform(1000),
	Self = self(),
	spawn_link(fun()-> init(Id, Rnd, Self) end).

init(Id, Rnd, Master) ->
	random:seed(Rnd,Rnd,Rnd),
	leader(Id, Master, []).

start(Id, Grp) ->
	Rnd =random:uniform(1000),
	Self = self(),
	spawn_link(fun()-> init(Id, Grp, Rnd, Self) end).

init(Id, Grp, Rnd, Master) ->
	random:seed(Rnd,Rnd,Rnd),
	Self = self(),
	
	Grp ! {join, Self},

	receive
		{view, State, Leader, Peers} ->
			
			Master ! {ok, State},
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, Peers)
	end.

slave(Id, Master, Leader, Peers) ->
	receive
		{mcast, Msg} ->
			Leader ! {mcast, Msg},
			slave(Id, Master, Leader, Peers);
		
		{'DOWN', _Ref, process, Leader, _Reason} ->
			election(Id, Master, Peers);
		
		{join, Peer} ->
			Leader ! {join, Peer},
			slave(Id, Master, Leader, Peers);
		
		{msg, Msg} ->
			Master ! {deliver, Msg},
			slave(Id, Master, Leader, Peers);
		
		{view, _, _, View} ->
			slave(Id, Master, Leader, View);
		
		stop ->
			ok;
		
		Error ->
			io:format("gms ~w: slave, strange message ~w~n", [Id, Error])
		
		after 1000 ->
			Master ! {error, "no reply from leader"}
	end.

election(Id, Master, [Leader|Rest]) ->
	if
		Leader == self() ->
			leader(Id, Master, Rest);
		true ->
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, Rest)
	end.

leader(Id, Master, Peers) ->
	receive
		{mcast, Msg} ->
			bcast(Id, {msg, Msg}, Peers),
			Master ! {deliver, Msg},
			leader(Id, Master, Peers);
		
		{join, Peer} ->
			Master ! request,
			joining(Id, Master, Peer, Peers);
		
		stop ->
			ok;
		
		Error ->
			io:format("gms ~w: leader, strange message ~w~n", [Id, Error])
	end.

joining(Id, Master, Peer, Peers) ->
	receive
		{ok, State} ->
			Peers2 = lists:append(Peers, [Peer]),
			bcast(Id, {view, State, self(), Peers2}, Peers2),
			leader(Id, Master, Peers2)
	end.

bcast(Id, Msg, Nodes) ->
	lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).

crash(Id) ->
	case random:uniform(?arghh) of
		?arghh ->
			io:format("leader ~w: crash~n", [Id]),
			exit(no_luck);
		_ ->
			ok
	end.