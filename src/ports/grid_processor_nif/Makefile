CC=gcc
CFLAGS += -O3 -fPIC -shared
LDFLAGS = -I/usr/local/lib/erlang/usr/include 
LINKFLAGS = 
SOURCES = ../map.c ../types.c ../nif_utils.c process_tile.c process_obj.c main_nif.c
EXECUTABLE = grid_processor.so
EBIN_DIR = ../../../bin/



all: clean $(SOURCES) $(EXECUTABLE) 
	
$(EXECUTABLE): $(SOURCES) 
	$(CC) $(CFLAGS) $(LDFLAGS) $(SOURCES) $(LINKFLAGS) -o $@
	cp $(EXECUTABLE) $(EBIN_DIR)

clean:
	rm -f $(EXECUTABLE)
