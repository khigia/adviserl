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
CMD_ERL      = erl

define run_test
	@echo
	@echo "**** Running test: $(strip $(1)) ****"
	@echo
	@${CMD_ERL} -noshell -pa ../ebin/ -pa ebin -kernel error_logger "{file, \"log/$(strip $(1))-kernel.log\"}" -sasl sasl_error_logger "{file, \"log/$(strip $(1))-sasl.log\"}" -s $(strip $(1)) start -s init stop
	@echo
endef

build:
	${CMD_ERL} -noshell -eval "make:all()" -s init stop

clean:
	@rm -rvf ebin/*.beam
	@rm -rvf log/*
	@rm -vf erl_crash.dump

run: run_basic run_load

run_basic: build
	${call run_test, basic}

run_load: build
	${call run_test, load}

