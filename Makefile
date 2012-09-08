PREFIX=/usr/local/

%.beam: %.erl
	erlc -W $<

MODS = parangone parangone_mod_http

all: compile

compile: ${MODS:%=%.beam} parangone

clean:
	rm -rf *.beam erl_crash.dump

install: all
	install -d -o root -g wheel -m 755 ${PREFIX}lib/parangone
	install -o root -g bin -m 555 ${MODS:%=%.beam} ${PREFIX}lib/parangone
	install -o root -g bin -m 555 parangone ${PREFIX}bin/

deinstall:
	rm -rf ${PREFIX}/lib/parangone
	rm ${PREFIX}/bin/parangone
