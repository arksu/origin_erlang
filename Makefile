ERLC = /usr/local/bin/erlc

EFLAGS += +optimize -smp

INCLUDE = include

#SOURCES_ALL = $(wildcard src/*.erl, src/mysql/*.erl)
#NIF = src/collision.erl src/cutils.erl src/grid_processor.erl src/visible.erl src/player_server.erl
#SOURCES += $(filter-out $(NIF),$(SOURCES_ALL))

SOURCES = $(wildcard src/*.erl src/mysql/*.erl src/game/*.erl)

OUTDIR = ebin_make
BEAMS = $(SOURCES:.erl=.beam)

all: prepare ebin_clean beam-compile ports

init:
	cp src/a1.cfg $(OUTDIR)

ebin_clean:
	rm -f $(OUTDIR)/*.beam

prepare:
	test -d $(OUTDIR) && echo "ebin_make ok" || mkdir $(OUTDIR)
	cp src/a1.app $(OUTDIR)
	cp src/server_start.sh $(OUTDIR)
	chmod +x $(OUTDIR)/server_start.sh
	cp src/server_start_detached.sh $(OUTDIR)
	chmod +x $(OUTDIR)/server_start_detached.sh
	echo prepared

ports:
	chmod +x make_ports.sh
	./make_ports.sh
	
beam-compile: $(BEAMS)

%.beam : %.erl
	$(ERLC) -W0 $(EFLAGS) -I $(INCLUDE) -o $(OUTDIR) $<
