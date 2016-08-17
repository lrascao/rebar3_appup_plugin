%% downgrade to Vsn, current state is passed
'$convert_state_record$'({down, _Vsn} = OldVsn, OldState, Extra0) ->
    %% the current version will become the old after the downgrade
    VsnData = {{vsn_data}},
    %% get the old/new vsn data
    OldVsnData = proplists:get_value(new, VsnData, []),
    NewVsnData = proplists:get_value(old, VsnData, []),
    %% get the old/new state record field definition
    OldRecordFields = proplists:get_value(state_record_fields, OldVsnData),
    NewRecordFields = proplists:get_value(state_record_fields, NewVsnData),
    Out0 = erlang:setelement(1, #'{{old_state_record_name}}'{},
                             {{state_record_name}}),
    NewState =
        lists:foldl(fun({Field, Pos}, Out) ->
                        %% is this a new field or an already existing one?
                        case proplists:get_value(Field, OldRecordFields) of
                            undefined ->
                                %% it's new field, set it's default value
                                erlang:setelement(Pos, Out,
                                                  erlang:element(Pos, Out0));
                            FromPos ->
                                %% it's an already existing field, copy it from the old record
                                erlang:setelement(Pos, Out,
                                                  erlang:element(FromPos, OldState))
                        end
                    end, Out0, NewRecordFields),
    Extra = Extra0 ++ [{old_state, OldState}],
    {OldVsn, NewState, Extra};
%% upgrade from OldVsn, old state is passed
'$convert_state_record$'(OldVsn, OldState, Extra0) ->
    VsnData = {{vsn_data}},
    %% get the old/new vsn data
    OldVsnData = proplists:get_value(old, VsnData, []),
    NewVsnData = proplists:get_value(new, VsnData, []),
    %% get the old/new state record field definition
    OldRecordFields = proplists:get_value(state_record_fields, OldVsnData),
    NewRecordFields = proplists:get_value(state_record_fields, NewVsnData),
    Out0 = #'{{state_record_name}}'{},
    NewState =
        lists:foldl(fun({Field, Pos}, Out) ->
                        %% is this a new field or an already existing one?
                        case proplists:get_value(Field, OldRecordFields) of
                            undefined ->
                                %% it's new field, set it's default value
                                erlang:setelement(Pos, Out,
                                                  erlang:element(Pos, Out0));
                            FromPos ->
                                %% it's an already existing field, copy it from the old record
                                erlang:setelement(Pos, Out,
                                                  erlang:element(FromPos, OldState))
                        end
                    end, Out0, NewRecordFields),
    Extra = Extra0 ++ [{old_state, OldState}],
    {OldVsn, NewState, Extra}.
