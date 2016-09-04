'$convert_state_record_aux$'(OldRecordFields, NewRecordFields, OldState, State0) ->
    lists:foldl(fun({Field, Pos}, StateAcc) ->
                    %% is this a new field or an already existing one?
                    case proplists:get_value(Field, OldRecordFields) of
                        undefined ->
                            %% this field doesn't exist in the old
                            %% record definition, it's new field, set it's default value
                            erlang:setelement(Pos, StateAcc,
                                              erlang:element(Pos, State0));
                        FromPos ->
                            %% it's an already existing field, copy it from the old record
                            erlang:setelement(Pos, StateAcc,
                                              erlang:element(FromPos, OldState))
                    end
                end, State0, NewRecordFields).
