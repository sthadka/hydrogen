REBAR=$(shell which rebar)

ifeq ($(REBAR),)
	REBAR = @$(shell pwd)/rebar3
endif
ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif

# Build tasks:

all: xref

compile:
	@$(REBAR) compile

test:
	@${REBAR} eunit

xref:
	@${REBAR} xref

dialyzer:
	@${REBAR} dialyzer

# Cleaning tasks:

clean:
	@$(REBAR) clean

# Other tasks:

shell: compile
	@$(REBAR) shell

doc:
	@$(REBAR) edoc

.PHONY: all test dialyzer clean allclean doc release qc qs start compile xref deps
