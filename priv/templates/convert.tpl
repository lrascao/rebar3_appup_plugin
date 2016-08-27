%% downgrade to Vsn, current state is passed
'$convert_state_record$'({down, _Vsn} = OldVsn, OldState, Extra0) ->
    %% the current version will become the old after the downgrade
    VsnData = {{vsn_data}},
    %% get the old/new vsn data, the new will become the old
    %% after the downgrade
    OldVsnData = proplists:get_value(new, VsnData, []),
    NewVsnData = proplists:get_value(old, VsnData, []),
    %% get the old/new state record fields definition
    OldRecordFields = proplists:get_value(state_record_fields, OldVsnData),
    NewRecordFields = proplists:get_value(state_record_fields, NewVsnData),
    %% the old state record name is still in the format 'state__<VSN>'
    %% we need to rename it
    State0 = erlang:setelement(1, #'{{old_state_record_name}}'{},
                               {{state_record_name}}),
    NewState = '$convert_state_record_aux$'(OldRecordFields, NewRecordFields,
                                            OldState, State0),
    Extra = Extra0 ++ [{old_state, OldState}],
    {OldVsn, NewState, Extra};
%% upgrade from OldVsn, old state is passed as argument
'$convert_state_record$'(OldVsn, OldState, Extra0) ->
    VsnData = {{vsn_data}},
    %% get the old/new vsn data
    OldVsnData = proplists:get_value(old, VsnData, []),
    NewVsnData = proplists:get_value(new, VsnData, []),
    %% get the old/new state record fields definition
    OldRecordFields = proplists:get_value(state_record_fields, OldVsnData),
    NewRecordFields = proplists:get_value(state_record_fields, NewVsnData),
    %% create an empty state record with defaults filled in
    State0 = #'{{state_record_name}}'{},
    NewState = '$convert_state_record_aux$'(OldRecordFields, NewRecordFields,
                                            OldState, State0),
    Extra = Extra0 ++ [{old_state, OldState}],
    {OldVsn, NewState, Extra}.
