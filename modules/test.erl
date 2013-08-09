%% Modified by: Alberto Lorente Leal, 
%% albll@kth.se
%% a.lorenteleal@gmail.com
%% Created: 24/09/2011
%% Description: Test module to launch the system

-module(test).


-export([start/0]).

start()->
	
	A=worker:start(leader,gms3,13,800),
	B=worker:start(b,gms3,13,A,800),
	C=worker:start(c,gms3,13,B,800),
	D=worker:start(d,gms3,13,C,800),
	E=worker:start(e,gms3,13,D,800),
	F=worker:start(f,gms3,13,E,500),
	G=worker:start(g,gms3,13,F,500),
	H=worker:start(h,gms3,13,G,500),
	I=worker:start(i,gms3,13,H,500),
	J=worker:start(i,gms3,13,I,500),
	K=worker:start(i,gms3,13,J,500),
	L=worker:start(i,gms3,13,K,500),
	M=worker:start(i,gms3,13,L,500),
	N=worker:start(i,gms3,13,M,500),
	O=worker:start(i,gms3,13,N,500).
	



