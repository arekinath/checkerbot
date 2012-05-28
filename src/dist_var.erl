-module(dist_var).
-export([new/2, dispatch/1]).

% derived from example in the documentation of SRFI27
% and translated to Erlang
-record( dist_state, {state, mu, sigma} ).

new(Mu, Sigma) ->
    new(false, Mu, Sigma).

% create the thunk
new(State, Mu, Sigma) ->
    This = #dist_state {
        state = State,
        mu = 1.0 * Mu,
        sigma = 1.0 * Sigma},
    IntPid = spawn(?MODULE, dispatch, [This]),
    fun () ->
        IntPid ! {self(), value},
        receive
            {retval, Any} -> Any
        end
    end.

dispatch(This) ->
    receive
        {Pid, value} ->
            {NewThis, Value} = value(This),
            Pid!{retval, Value},
            dispatch(NewThis)
    end.

value(This) ->
    case This#dist_state.state of
        true -> Val = This#dist_state.mu
                    + (This#dist_state.sigma * This#dist_state.state),
                {This#dist_state{state = false}, Val};
        _    -> sigma_loop(This)
    end.

sigma_loop(This) ->
    V1 = 2.0 * random:uniform() - 1.0,
    V2 = 2.0 * random:uniform() - 1.0,
    S  = (V1 * V1) + (V2 * V2),
    if
        S >= 1.0 ->
            sigma_loop(This);
        true     ->
            Scale = math:sqrt( (-2.0 * math:log(S)) / S),
            Val = This#dist_state.mu
                  + (This#dist_state.sigma * Scale * V1),
            {This#dist_state{state = Scale * V2}, Val}
    end.
