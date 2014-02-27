%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    Tellstick net local access (need release 17!)
%%% @end
%%% Created : 27 Feb 2014 by Tony Rogvall <tony@rogvall.se>

-module(tellstick_net).

-compile(export_all).

-record(tsock,
	{
	  socket,
	  ip,
	  port
	}).

-define(LOCAL_PORT, 42314).

%% encode "json" data in telldus format
encode(X) when is_integer(X) ->
    ["i",integer_to_list(X,16),"s"];
encode(X) when is_atom(X) ->
    encode(atom_to_list(X));
encode(X) when is_list(X); is_binary(X) ->
    Sz = erlang:iolist_size(X),
    [integer_to_list(Sz,16),":",X];
encode({array,Xs}) ->
    ["l", [encode(X) || X <- Xs], "s"];
encode({struct,Xs}) ->
    ["h", [[encode(K),encode(V)] || {K,V} <- Xs], "s"].


decode(Cs) ->
    case decode_(Cs) of
	{X,[]} -> X
    end.

%% decode a sequence of data (not list nor dictionary)
decode_sequence(Cs) ->
    case decode_(Cs) of
	{X,[]} -> [X];
	{X,Cs1} -> [X | decode_sequence(Cs1)]
    end.

%% decode (telldus?) encode json structure
decode_([$i|Cs]) -> decode_int(Cs);
decode_([$l|Cs]) -> decode_array(Cs,[]);
decode_([$h|Cs]) -> decode_struct(Cs,[]);
decode_(Cs) ->
    {Sz,[$:|Cs1]} = decode_hex(Cs,0),
    lists:split(Sz, Cs1).

decode_int([$-|Cs]) ->
    {I,[$s|Cs1]} = decode_hex(Cs,0),
    {-I,Cs1};
decode_int(Cs) ->
    {I,[$s|Cs1]} = decode_hex(Cs,0),
    {I,Cs1}.

decode_array([$s|Cs],Acc) -> 
    {{array,lists:reverse(Acc)}, Cs};
decode_array(Cs, Acc) ->
    {E, Cs1} = decode_(Cs),
    decode_array(Cs1, [E|Acc]).

decode_struct([$s|Cs],Acc) -> 
    {{struct,lists:reverse(Acc)}, Cs};
decode_struct(Cs, Acc) ->
    {K, Cs1} = decode_(Cs),
    {V, Cs2} = decode_(Cs1),
    decode_struct(Cs2, [{list_to_atom(K),V}|Acc]).

decode_hex([X|Xs],I) when X >= $0, X =< $9 -> 
    decode_hex(Xs, I*16+(X-$0));
decode_hex([X|Xs],I) when X >= $A, X =< $F ->
    decode_hex(Xs, I*16 + ((X-$A)+10));
decode_hex(Xs, I) -> {I, Xs}.



open() ->
    case detect() of
	{ok,[{Ip,_} | _]} ->
	    open(Ip);
	Error -> Error
    end.

open(Ip) ->
    case gen_udp:open(0, [{active,true}]) of
	{ok,U} -> {ok,#tsock{socket=U,ip=Ip,port=?LOCAL_PORT}};
	Error -> Error
    end.

close(#tsock{socket=U}) ->
    gen_udp:close(U).

send(#tsock{socket=U,ip=Ip,port=Port}, Data) ->
    gen_udp:send(U, Ip, Port, Data).


%% disconnect and reset the tellstick-net
disconnect(TSock) ->
    send(TSock, encode(disconnect)).

reglistener(TSock) ->
    send(TSock,encode(reglistener)).

%% {struct, [
%%     {"P",Pause}     (default 11)
%%     {"R",Repeats}   (default 10)
%%     {"S", Data}
%% }
%%
send_items(TSock, Items) ->
    send(TSock, [encode(send),encode({struct,Items})]).

%% {struct, [
%%     {"P",Pause}     (default 11)
%%     {"R",Repeats}   (default 10)
%%     {"protocol","arctech"}, {"model","selflearning"}, 
%%         {"house", H}, {"unit", U}, {"method", M}
%% }
%% zero based house=0 unit=0, value=0 == nexa $A,1,true
send_archtech(TSock, House, Unit, Value) ->
    Items = [{protocol,"arctech"},
	     {model,"selflearning"},
	     {house, House},
	     {unit, Unit},
	     {method,Value}],
    send_items(TSock, Items).

send_pulses(TSock, Data) ->
    send_items(TSock, [{"S",Data}]).

%%
%% detect tellsticks on the local net
%% broadcast on all interfaces
%%
detect() ->
    detect(3, 1).

detect(Retry, Expect) ->
    Timeout = 100,
    case broadcast_address_list() of
	{ok,AddrList} ->
	    case gen_udp:open(0, [{broadcast, true},{active,true}]) of
		{ok, U} ->
		    try detect(U,AddrList, Retry, Timeout, Expect, []) of
			Res -> Res
		    after
			gen_udp:close(U)
		    end;
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

detect(_U, _AList, 0, _Timeout, _Expect, Acc) ->
    if Acc =:= [] ->
	    {error, timeout};
       true ->
	    {ok,Acc}
    end;
detect(U, AList, Retry, Timeout, Expect, Acc) ->
    lists:foreach(fun(Addr) -> gen_udp:send(U, Addr, 30303, "D") end, AList),
    case collect(U, Timeout, Acc) of
	Acc ->
	    detect(U, AList, Retry-1, Timeout, Expect, Acc);
	Acc1 ->
	    N = length(Acc1),
	    if N >= Expect ->
		    {ok, Acc1};
	       true ->
		    detect(U, AList, Retry-1, Timeout, Expect, Acc)
	    end
    end.

collect(U, Timeout, Acc) ->
    receive
	{udp,U,TellStickIp,30303,Data} ->
	    case lists:keymember(TellStickIp,1,Acc) of
		true ->
		    collect(U, Timeout, Acc);
		false ->
		    case string:tokens(Data, ":") of
			[Product,Mac,Code,Version] -> %% handle more variants?
			    Stick = {TellStickIp,
				   [{product,Product},
				    {mac,Mac},
				    {code,Code},
				    {version,Version}]},
			    collect(U, Timeout, [Stick|Acc]);
			_ ->
			    lager:warning("unknown form ~p", [Data]),
			    collect(U, Timeout, Acc)
		    end
	    end
    after Timeout ->
	    Acc
    end.

%% find a list of possible broadcast addresses
broadcast_address_list() ->
    case inet:getifaddrs() of
	{ok,List} ->
	    {ok, 
	     lists:foldl(
	       fun({_Name,Flags}, Acc) ->
		       case proplists:get_value(broadaddr,Flags) of
			   undefined -> Acc;
			   IPv4={_A,_B,_C,_D} -> [IPv4|Acc];
			   _ -> Acc
		       end
	       end, [], List)};
	Error ->
	    Error
    end.

			      
