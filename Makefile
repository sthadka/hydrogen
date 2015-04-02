REBAR=$(shell which rebar)

ifeq ($(REBAR),)
	REBAR = @$(shell pwd)/rebar
endif
ifeq ($(REBAR),)
$(error "Rebar not available on this system")
endif
ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/deps/*/ebin
PLT=$(CURDIR)/.hydrogen.plt
ERL = $(shell which erl)

# Build tasks:

all: xref

deps:
	@${REBAR} get-deps
	@${REBAR} compile

compile: deps
	@$(REBAR) skip_deps=true compile

qc:
	@$(REBAR) skip_deps=true compile

test:
	@${REBAR} eunit skip_deps=true

xref: compile
	@${REBAR} skip_deps=true xref

dialyzer:
	@dialyzer --fullpath --plt $(PLT) --src src -r ./ebin \
		-Wrace_conditions -Wunderspecs -Wspecdiffs

plt: compile
	@dialyzer --output_plt $(PLT) --build_plt --apps \
		erts kernel stdlib crypto sasl ssl inets xmerl public_key compiler \
		tools runtime_tools ebin

# Cleaning tasks:

depclean:
	@rm -rf deps/*

clean:
	$(REBAR) clean skip_deps=true

allclean: depclean
	@${REBAR} clean

# Other tasks:

start: compile
	@$(ERL) $(ERLFLAGS)

qs: qc
	@$(ERL) $(ERLFLAGS)

doc:
	@$(REBAR) skip_deps=true doc

.PHONY: all test dialyzer clean allclean doc release qc qs start compile xref
