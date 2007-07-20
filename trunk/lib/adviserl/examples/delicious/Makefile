
CMD_ERL      = erl
ERLPATH      = -pa ebin
BUILDOPT_DEV = debug_info, {d,'EUNIT'}, {d,'LOG_DEBUG'}

.PHONY : all
all: dev

.PHONY : dev
dev:
	${CMD_ERL} -noshell ${ERLPATH} -eval "make:all([${BUILDOPT_DEV}])" -s erlang halt

.PHONY : clean
clean:
	@rm -vf erl_crash.dump
	@rm -vf ebin/*.beam
	@rm -vf log/*
	
