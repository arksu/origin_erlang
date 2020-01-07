%% Author: arksu
%% Created: 20.01.2012
%% Description: TODO: Add description to randoms
-module(randoms).

-export([get_string/0, get/0, get/1]).

-export([start_link/0, init/0]).


start_link() ->
	P = spawn_link(?MODULE, init, []),
    register(randoms, P),
    {ok, P}.

init() ->
    {A1, A2, A3} = now(),
    random:seed(A1,A2,A3),
    loop().

loop() ->
    receive
	{From, get_random, N} ->
	    From ! {random, random:uniform(N)},
	    loop();
	_ ->
	    loop()
    end.

get_string() ->
    randoms ! {self(), get_random, 65536*65536},
    receive
	{random, R} -> integer_to_list(R)
	after 5000 -> exit(timeout_randoms)
    end.

get() ->
	randoms ! {self(), get_random, 65536*65536},
    receive
	{random, R} -> R
	after 5000 -> exit(timeout_randoms)
    end.

get(N) ->
	randoms ! {self(), get_random, N},
    receive
	{random, R} -> R
	after 5000 -> exit(timeout_randoms)
    end.