# Copyright 2012 Erlware, LLC. All Rights Reserved.
#
# This file is provided to you under the Apache License,
# Version 2.0 (the "License"); you may not use this file
# except in compliance with the License.  You may obtain
# a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.
#

# =============================================================================
# Verify that the programs we need to run are installed on this system
# =============================================================================
REBAR3=./rebar3
ifeq ($(REBAR3),)
$(error "Rebar3 not available on this system")
endif

.PHONY: all compile clean dialyzer test

travis: test
all: deps compile

# =============================================================================
# Rules to build the system
# =============================================================================

$(REBAR3):
	wget https://s3.amazonaws.com/rebar3/rebar3
	chmod u+x rebar3

deps: $(REBAR3)
	- $(REBAR3) compile

compile: $(REBAR3)
	- $(REBAR3) compile

clean: $(REBAR3)
	- $(REBAR3) clean

test: compile
	rm -rf _build/test
	$(REBAR3) ct

dialyzer: $(REBAR3)
	- $(REBAR3) dialyzer

distclean: clean
	- rm -rf .rebar3
	- rm -rf ebin
	- rm -rf _build
	- $(REBAR3) clean

rebuild: distclean compile dialyze
