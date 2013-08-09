%% Author: Alberto Lorente Leal, 
%% albll@kth.se
%% a.lorenteleal@gmail.com
%% Created: 24/09/2011
%% Description: Based on the gsm2 code. 
%% In this version we add reliable multicast
-module(gms3).

-export([start/1, start/2]).

-define(arghh,100).

start(Id) ->
	Rnd =random:uniform(1000),
	Self = self(),
	spawn_link(fun()-> init(Id, Rnd, Self) end).

init(Id, Rnd, Master) ->
	random:seed(Rnd,Rnd,Rnd),
	leader(Id, Master, 0, []).

start(Id, Grp) ->
	Rnd =random:uniform(1000),
	Self = self(),
	spawn_link(fun()-> init(Id, Grp, Rnd, Self) end).

init(Id, Grp, Rnd, Master) ->
	random:seed(Rnd,Rnd,Rnd),
	Self = self(),
	
	Grp ! {join, Self},

	receive
		{view, N, State, Leader, Peers} ->
			
			Master ! {ok, State},
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, N+1, {view, N, State, Leader, Peers},  Peers)
	end.

slave(Id, Master, Leader, N, Last, Peers) ->
	receive
		{mcast, Msg} ->
			Leader ! {mcast, Msg},
			slave(Id, Master, Leader, N,Last, Peers);
		
		{'DOWN', _Ref, process, Leader, _Reason} ->
			election(Id, Master, N,Last, Peers);
		
		{join, Peer} ->
			Leader ! {join, Peer},
			slave(Id, Master, Leader,N,Last, Peers);
		
		{msg, N, Msg} ->
			Master ! {deliver, Msg},
			slave(Id, Master, Leader, N+1, {msg, N, Msg} , Peers);
		
		{view, N, State,Leader, View} ->
			slave(Id, Master, Leader, N+1, {view, N, State,Leader, View}, View);
		
		{msg, I, _} when I < N ->
			slave(Id, Master, Leader, N, Last, Peers);
		
		stop ->
			ok;
		
		Error ->
			io:format("gms ~w: slave, strange message ~w~n", [Id, Error])
		
		after 1000 ->
			Master ! {error, "no reply from leader"}
	end.

election(Id, Master, N,Last, [Leader|Rest]) ->
	if
		Leader == self() ->
			bcast(Id,Last,Rest),
			leader(Id, Master, N, Rest);
		true ->
			erlang:monitor(process, Leader),
			slave(Id, Master, Leader, N,Last, Rest)
	end.

leader(Id, Master, N, Peers) ->
	receive
		{mcast, Msg} ->
			bcast(Id, {msg, N, Msg}, Peers),
			Master ! {deliver, Msg},
			leader(Id, Master, N+1, Peers);
		
		{join, Peer} ->
			Master ! request,
			joining(Id, Master, N, Peer, Peers);
		
		stop ->
			ok;
		
		Error ->
			io:format("gms ~w: leader, strange message ~w~n", [Id, Error])
	end.

joining(Id, Master, N, Peer, Peers) ->
	receive
		{ok, State} ->
			Peers2 = lists:append(Peers, [Peer]),
			bcast(Id, {view, N, State, self(), Peers2}, Peers2),
			leader(Id, Master, N+1, Peers2)
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