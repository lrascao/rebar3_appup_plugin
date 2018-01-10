%% -------------------------------------------------------------------
%%
%% Copyright (c) 2016 Luis Rascão.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(rebar3_appup_plugin_eunit_tests).

-include_lib("eunit/include/eunit.hrl").

matching_version1_test() ->
  test_matching(".*", "1.0.2", true).

matching_version2_test() ->
  test_matching("1.*", "1.0.2", true).

matching_version3_test() ->
  test_matching("*.*.2", "1.0.2", true).

matching_version4_test() ->
  test_matching("*.*.3", "1.0.2", false).

%% Check exact versions
pattern1_test() ->
  test_matching_patterns(
    {"1.0.34",
     [{"1.0.33", [{apply, {io, format, ["Upgrading started from 1.0.33 to 1.0.34"]}}]}],
     [{"1.0.33", [{apply, {io, format, ["Downgrading started from 1.0.34 to 1.0.33"]}}]}]},
    {"1.0.34",
     [{"1.0.33", [{apply, {io, format, ["Upgrading finished from 1.0.33 to 1.0.34"]}}]}],
     [{"1.0.33", [{apply, {io, format, ["Downgrading finished from 1.0.34 to 1.0.33"]}}]}]},
    "1.0.33", "1.0.34",
    [{apply,{io,format,["Upgrading is in progress..."]}}],
    [{apply,{io,format,["Downgrading is in progress..."]}}],
    {[{apply,{io,format,["Upgrading started from 1.0.33 to 1.0.34"]}},
      {apply,{io,format,["Upgrading is in progress..."]}},
      {apply,{io,format,["Upgrading finished from 1.0.33 to 1.0.34"]}}],
     [{apply,{io,format,["Downgrading started from 1.0.34 to 1.0.33"]}},
      {apply,{io,format,["Downgrading is in progress..."]}},
      {apply,{io,format,["Downgrading finished from 1.0.34 to 1.0.33"]}}]}
   ).

%% New ver is a pattern
pattern2_test() ->
  test_matching_patterns(
    {".*",
     [{"1.0.33", [{apply, {io, format, ["Upgrading started from 1.0.33 to 1.*"]}}]}],
     [{"1.0.33", [{apply, {io, format, ["Downgrading started from 1.* to 1.0.33"]}}]}]},
    {"1.*",
     [{"1.0.33", [{apply, {io, format, ["Upgrading finished from 1.0.33 to 1.*"]}}]}],
     [{"1.0.33", [{apply, {io, format, ["Downgrading finished from 1.* to 1.0.33"]}}]}]},
    "1.0.33", "1.0.34",
    [{apply,{io,format,["Upgrading is in progress..."]}}],
    [{apply,{io,format,["Downgrading is in progress..."]}}],
    {[{apply,{io,format,["Upgrading started from 1.0.33 to 1.*"]}},
      {apply,{io,format,["Upgrading is in progress..."]}},
      {apply,{io,format,["Upgrading finished from 1.0.33 to 1.*"]}}],
     [{apply,{io,format,["Downgrading started from 1.* to 1.0.33"]}},
      {apply,{io,format,["Downgrading is in progress..."]}},
      {apply,{io,format,["Downgrading finished from 1.* to 1.0.33"]}}]}
   ).

%% Both new and old versions are patterns.
%% Check upgrade between 1.* <--> 2.*
pattern3_test() ->
  test_matching_patterns(
    {"2.*",
     [{"1.*", [{apply, {io, format, ["Upgrading started from 1.* to 2.*"]}}]}],
     [{"1.*", [{apply, {io, format, ["Downgrading started from 2.* to 1.*"]}}]}]},
    {"2.*",
     [{"1.*", [{apply, {io, format, ["Upgrading finished from 1.* to 2.*"]}}]}],
     [{"1.*", [{apply, {io, format, ["Downgrading finished from 2.* to 1.*"]}}]}]},
    "1.0.33", "2.0.1",
    [{apply,{io,format,["Upgrading is in progress..."]}}],
    [{apply,{io,format,["Downgrading is in progress..."]}}],
    {[{apply,{io,format,["Upgrading started from 1.* to 2.*"]}},
      {apply,{io,format,["Upgrading is in progress..."]}},
      {apply,{io,format,["Upgrading finished from 1.* to 2.*"]}}],
     [{apply,{io,format,["Downgrading started from 2.* to 1.*"]}},
      {apply,{io,format,["Downgrading is in progress..."]}},
      {apply,{io,format,["Downgrading finished from 2.* to 1.*"]}}]}
   ).

%% Post conditions are not matching the new version.
%% They should not be present in the generated appup file
pattern4_test() ->
  test_matching_patterns(
    {"2.*",
     [{"1.*", [{apply, {io, format, ["Upgrading started from 1.* to 2.*"]}}]}],
     [{"1.*", [{apply, {io, format, ["Downgrading started from 2.* to 1.*"]}}]}]},
    {"3.*",    %% intentionally 3.*.... This part won't be in the generated appup
     [{"1.*", [{apply, {io, format, ["Upgrading finished from 1.* to 2.*"]}}]}],
     [{"1.*", [{apply, {io, format, ["Downgrading finished from 2.* to 1.*"]}}]}]},
    "1.0.33", "2.0.1",
    [{apply,{io,format,["Upgrading is in progress..."]}}],
    [{apply,{io,format,["Downgrading is in progress..."]}}],
    {[{apply,{io,format,["Upgrading started from 1.* to 2.*"]}},
      {apply,{io,format,["Upgrading is in progress..."]}}
      %% Post instructions are missing
     ],
     [{apply,{io,format,["Downgrading started from 2.* to 1.*"]}},
      {apply,{io,format,["Downgrading is in progress..."]}}
      %% Post instructions are missing
     ]}
   ).

test_matching_patterns(PreContents, PostContents, OldVer, NewVer,
                       UpgradeInstructions, DowngradeInstructions, Expected) ->
    Actual = rebar3_appup_generate:merge_instructions(
               PreContents, PostContents, OldVer, NewVer, UpgradeInstructions, DowngradeInstructions),
    ?assertEqual(Expected, Actual).

test_matching(Pattern, Version, Expected) ->
  Actual = rebar3_appup_generate:matching_versions(Pattern, Version),
  ?assertEqual(Expected, Actual).
