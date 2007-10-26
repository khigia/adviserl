
CMD_ERL      = erl
ERLPATH      = -pa ebin
BUILDOPT_DEV = debug_info, {d,'EUNIT'}, {d,'LOG_DEBUG'}

.PHONY : prodbuild
prodbuild : BUILDOPT = {d,'LOG_DEBUG'}
prodbuild : build

.PHONY : devbuild
devbuild : BUILDOPT = debug_info, {d,'EUNIT'}, {d,'LOG_DEBUG'}
devbuild : build

.PHONY : build
build:
	${CMD_ERL} -noshell ${ERLPATH} -eval "make:all([${BUILDOPT}])" -s erlang halt

.PHONY : clean
clean:
	@rm -vf erl_crash.dump
	@rm -vf ebin/*.beam
	@rm -vf log/*
	
