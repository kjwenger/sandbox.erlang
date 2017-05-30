-module(kvs_server).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([put/2, get/1, delete/1]).

-record(state, {
    tab
}).

start_link() ->
    error_logger:info_msg("starting kvs server~n", []),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

put(Key, Value) ->
    error_logger:info_msg("putting into kvs server: ~p/~p~n", [Key, Value]),
    gen_server:call(?MODULE, {put, Key, Value}).

get(Key) ->
    error_logger:info_msg("getting from kvs server: ~p~n", [Key]),
    gen_server:call(?MODULE, {get, Key}).

delete(Key) ->
    error_logger:info_msg("deleting from kvs server: ~p~n", [Key]),
    gen_server:call(?MODULE, {delete, Key}).

% --- callbacks ---

handle_call({put, Key, Value}, _From, #state{tab = Tab} = State) ->
    Reply = ets:insert(Tab, {Key, Value}),
    {reply, Reply, State};

handle_call({get, Key}, _From, #state{tab = Tab} = State) ->
    Reply = ets:lookup(Tab, Key),
    case Reply of
        [] -> {reply, not_found, State};
        [{Key, Value}] -> {reply, Value, State};
        _ -> {reply, not_found, State}
    end;

handle_call({delete, Key}, _From, #state{tab = Tab} = State) ->
    Reply = ets:delete(Tab, Key),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

init([]) ->
    error_logger:info_msg("initializing kvs server~n", []),
    {ok, #state{tab = ets:new(kvs, [set, private])}}.

terminate(_Reason, _State) ->
    error_logger:info_msg("terminating kvs server~n", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    error_logger:info_msg("code change for kvs server~n", []),
    {ok, State}.
