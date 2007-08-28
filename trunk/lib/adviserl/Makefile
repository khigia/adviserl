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

#TODO test and vendor need to be included in this makefile
# reason: damn slow to run 3 erlang VM just to see if something has to be rebuilt!
# see: http://miller.emu.id.au/pmiller/books/rmch/
#TODO some files need to be make-configured (e.g. app.src to build app, env_dev, inets.conf...)

# Configuration

CMD_ERL      = erl
CMD_DIALYZER = dialyzer
APPNAME      = $(shell pwd | xargs basename | sed -e 's/-.*$$//')

ERLPATH=-pa ebin -pa vendor/array/ebin

BUILDOPT_PROD={smp, enable}
BUILDOPT_DEV=debug_info, {d,'EUNIT'}, {d,'LOG_DEBUG'}

DOCOPT={todo, true}, {private, true}


# Main targets

.PHONY : all
all: prodbuild doc

.PHONY : prodbuild
prodbuild: subtargets_build
	@echo "==== Compiling application ===="
	${CMD_ERL} -noshell ${ERLPATH} -eval 'make:all([${BUILDOPT_PROD}])' -s init stop
	@echo

.PHONY : doc
doc:
	@echo "==== Compiling documentation ===="
	${CMD_ERL} -noshell ${ERLPATH} -eval 'edoc:application(${APPNAME}, ".", [${DOCOPT}]).' -s init stop
	@echo


# Development targets

.PHONY : doc
dev: devbuild test checks doc

.PHONY : logs
logs:
	@echo "==== Extracting logs ===="
	${CMD_ERL} -noshell ${ERLPATH} -boot start_sasl -eval "rb:start([{report_dir, \"./log/\"}]), rb:show()." -s init stop
	@echo

.PHONY : devbuild
devbuild: subtargets_build
	@echo "==== Compiling application ===="
	${CMD_ERL} -noshell ${ERLPATH} -eval "up_to_date = make:all([${BUILDOPT_DEV}])." -s init stop
	@echo

.PHONY : test
test: devbuild
	@echo "==== Testing application (require eunit) ===="
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
	@echo

.PHONY : checks
checks: xref dialyzer cover

.PHONY : xref
xref:
	@echo "==== Checking code: xref ===="
	${CMD_ERL} -noshell ${ERLPATH} -eval 'io:format("~p~n", [xref:d("ebin")]).' -s init stop
	@echo

.PHONY : dialyzer
dialyzer:
	@echo "==== Checking code: dialyzer ===="
	${CMD_DIALYZER} ${ERLPATH} -I include --src -c src
	@echo

.PHONY : cover
cover:
	@echo "==== Checking code: cover ===="
	@echo "... nothing yet!"
	@echo

.PHONY : perfs
perfs: fprof

.PHONY : fprof
fprof:
	@echo "==== Testing performance: fprof ===="
	@echo "... nothing yet!"
	@echo

.PHONY : clean
clean: subtargets_clean
	@echo "==== Removing built files ===="
	@rm -vf erl_crash.dump
	@rm -vf ebin/*.beam
	@rm -vf test/*.beam
	@rm -vf log/*
	@touch doc/remove.me # ensure next command does not failed
	@ls -1 -I *.edoc -I *.txt -I figure doc | sed -e 's#\(.*\)#doc/\1#' | xargs rm -v
	@echo


# External targets

.PHONY : subtargets_build
subtargets_build:
	@echo "==== Building dependencies ===="
	@cd vendor ; make
	@cd test ; make build
	@echo
	
.PHONY : subtargets_clean
subtargets_clean:
	@echo "==== Removing built files ===="
	@cd vendor ; make clean
	@cd test ; make clean
	@echo
