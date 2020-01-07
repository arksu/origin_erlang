CC=gcc
CFLAGS += -O3 -fPIC -shared
LDFLAGS = -I/usr/local/lib/erlang/usr/include 
LINKFLAGS = 
SOURCES = ../types.c ../nif_utils.c ../map.c ../tiles.c cutils.c
EXECUTABLE = cutils.so
EBIN_DIR = ../../../bin/



all: clean $(SOURCES) $(EXECUTABLE) 
	
$(EXECUTABLE): $(SOURCES) 
	$(CC) $(CFLAGS) $(LDFLAGS) $(SOURCES) $(LINKFLAGS) -o $@
	cp $(EXECUTABLE) $(EBIN_DIR)

clean:
	rm -f $(EXECUTABLE)
	
