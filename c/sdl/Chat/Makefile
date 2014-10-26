CFLAGS:= -Wall -std=c11
LIBS:= $(shell sdl-config --libs) $(shell sdl-config --cflags) -lSDL_net


all:
	cc host.c util.c -o host $(CFLAGS) $(LIBS) && \
	cc join.c util.c -o join $(CFLAGS) $(LIBS)
