REBAR = ./rebar -j8
ANSIBLE-PLAYBOOK = ansible-playbook

all: deps compile

compile: deps
	${REBAR} compile

deps:
	${REBAR} get-deps

clean:
	${REBAR} clean

generate: compile
	cd rel && ../${REBAR} generate -f

relclean:
	rm -rf rel/erest

run: generate
	./rel/erest/bin/erest start

console: generate
	./rel/erest/bin/erest console

foreground: generate
	./rel/erest/bin/erest foreground

erl: compile
	erl -pa ebin/ -pa lib/*/ebin/ -s erest

test:
	ERL_AFLAGS="-config ${PWD}/rel/erest/etc/app.config -mnesia dir \"'data/'\""  $(REBAR)  compile ct suite=erest skip_deps=true

it_local_deploy:
	${ANSIBLE-PLAYBOOK} ${PWD}/it_deploy/it_play.yml -K

integration_test:
	ERL_AFLAGS="-config ${PWD}/rel/files/erest_it.config  -mnesia dir \"'data/'\" " $(REBAR)  compile ct suite=it -v skip_deps=true

.PHONY: all deps test clean
