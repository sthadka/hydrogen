REBAR = @$(shell pwd)/rebar

# Build tasks:

all: xref

dependencies:
	@${REBAR} get-deps

compile: dependencies
	@$(REBAR) skip_deps=true compile

qc:
	@$(REBAR) skip_deps=true compile

test:
	@${REBAR} eunit skip_deps=true

xref: compile
	@${REBAR} skip_deps=true xref

dialyzer:
	@dialyzer --plt .honcho.plt --src src \
		-Wrace_conditions -Wunderspecs -Wspecdiffs

plt: compile
	@dialyzer --output_plt .honcho.plt --build_plt --apps \
		erts kernel stdlib crypto sasl ssl inets xmerl public_key compiler \
		tools runtime_tools deps/*/ebin ebin

# Cleaning tasks:

depclean:
	@rm -rf deps/*

clean:
	$(REBAR) clean skip_deps=true

allclean: depclean
	@${REBAR} clean

# Other tasks:

start: compile
	erl -pa deps/*/ebin ebin

qs: qc
	erl -pa deps/*/ebin ebin

doc:
	$(REBAR) doc skip_deps=true

.PHONY: all test dialyzer clean allclean doc release qc start compile
