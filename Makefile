PREFIX=/usr/local/

.SUFFIXES: .erl .beam

.erl.beam:
	erlc -W $<

MODS = parangonebla parangone_mod_httpbla

all: compile

compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump

install: all
	install -d -o root -g wheel -m 755 ${PREFIX}lib/parangone
	install -o root -g bin -m 555 ${MODS:%=%.beam} ${PREFIX}lib/parangone
	install -o root -g bin -m 555 parangone.sh ${PREFIX}bin/parangone

deinstall:
	rm -rf ${PREFIX}/lib/parangone
	rm ${PREFIX}/bin/parangone