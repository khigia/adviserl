#===========================================================================
# This file is part of adviserl.
#
# adviserl is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# adviserl is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with adviserl; if not, write to the Free Software
# Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
#===========================================================================
# Description: ad-hoc (Linux) Makefile for standard Erlang/OTP application
# Author: Ludovic Coquelle <lcoquelle@gmail.com>

#TODO some files need to be make-configured (e.g. app.src to build app, env_dev, inets.conf...)

# Configuration

CMD_ERL      = erl
CMD_ERLC     = erlc
CMD_DIALYZER = dialyzer
APPNAME      = $(shell pwd | xargs basename | sed -e 's/-.*$$//')

ERLPATH=-pa ebin -pa vendor/array/ebin

DOCOPT={todo, true}, {private, true}


APP_MOD    = $(patsubst src/%.erl,ebin/%.beam,$(wildcard src/*.erl))
VENDOR_MOD = $(patsubst vendor/array/src/%.erl,vendor/array/ebin/%.beam,$(wildcard vendor/array/src/*.erl))
TEST_MOD   = $(patsubst test/src/%.erl,test/ebin/%.beam,$(wildcard test/src/*.erl))


# Main targets

.PHONY : prodbuild
prodbuild : BUILDOPT = -smp
prodbuild : vendor_build ${APP_MOD}

.PHONY : doc
doc:
	${CMD_ERL} \
        -noshell \
        ${ERLPATH} \
        -eval 'edoc:application(${APPNAME}, ".", [${DOCOPT}]).' \
        -s init stop

.PHONY : logs
logs:
	${CMD_ERL} \
        -noshell \
        ${ERLPATH} \
        -boot start_sasl \
        -eval "(rb:start([{report_dir, \"./log/\"}]) =:= ok) andalso rb:show()." \
        -s init stop

.PHONY : clean
clean :
	@rm -vf erl_crash.dump
	@rm -vf $(APP_MOD)
	@rm -vf $(VENDOR_MOD)
	@rm -vf $(TEST_MOD)
	@rm -vf log/*
	@touch doc/remove.me # ensure next command does not failed
	@ls -1 -I *.edoc -I *.txt -I figure doc \
        | sed -e 's#\(.*\)#doc/\1#' \
        | xargs rm -v


# Development targets

.PHONY : dev
dev: devbuild regression checks doc

.PHONY : devbuild
devbuild: BUILDOPT = +debug_info -DEUNIT -DLOG_DEBUG -Wall
devbuild: vendor_build ${APP_MOD} test_build

.PHONY : test
test: test_basic test_load

.PHONY : regression
regression: devbuild
	@erl \
		-noshell \
		-kernel error_logger      "{file, \"log/utest-kernel.log\"}" \
		-sasl   sasl_error_logger "{file, \"log/utest-sasl.log\"}" \
		${ERLPATH} \
		-eval "filelib:fold_files(\
			\"ebin\",\
			\".*\.beam\",\
			true,\
			fun(F, Acc) -> \
				M = list_to_atom(filename:basename(F, \".beam\")),\
				io:format(\"Testing ~p~n\", [M]),\
				TR = eunit:test(M, [{verbose,false}]),\
				Acc\
			end,\
			[]\
		)." \
		-s init stop

.PHONY : checks
checks: xref dialyzer cover

.PHONY : xref
xref:
	${CMD_ERL} -noshell ${ERLPATH} -eval 'io:format("~p~n", [xref:d("ebin")]).' -s init stop

.PHONY : dialyzer
dialyzer:
	${CMD_DIALYZER} ${ERLPATH} -I include --src -c src

.PHONY : cover
cover:
	@echo "... cover not used!"

.PHONY : perfs
perfs: fprof

.PHONY : fprof
fprof:
	@echo "... fprof not used!"



# Sub-targets

.PHONY : vendor_build
vendor_build : BUILDOPT = -DNOTEST
vendor_build: ${VENDOR_MOD}

.PHONY : test_build
test_build: BUILDOPT = +debug_info -DEUNIT -DLOG_DEBUG -Wall
test_build: ${TEST_MOD}


# Compiling

ebin/%.beam : src/%.erl
	${CMD_ERLC} ${BUILDOPT} -o ebin $<

test/ebin/%.beam : test/src/%.erl
	${CMD_ERLC} ${BUILDOPT} -o test/ebin $<

vendor/array/ebin/%.beam : vendor/array/src/%.erl
	${CMD_ERLC} ${BUILDOPT} -o vendor/array/ebin $<


# Testing

define run_test
	@echo
	@echo "**** Running test: $(strip $(1)) ****"
	@echo
	@${CMD_ERL} \
        -noshell \
        -config ebin/env_test \
        -pa ebin \
        -pa test/ebin/ \
        -kernel error_logger "{file, \"log/$(strip $(1))-kernel.log\"}" \
        -sasl sasl_error_logger "{file, \"log/$(strip $(1))-sasl.log\"}" \
        -s $(strip $(1)) start \
        -s init stop
	@echo
endef

.PHONY : test
test_basic: devbuild test_build
	${call run_test, basic}

.PHONY : test
test_load: devbuild test_build
	${call run_test, load}
