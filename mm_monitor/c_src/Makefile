ERL_INTERFACE_VER=3.5.9

CFLAGS =	-g -I/opt/erlang/lib/erlang/lib/erl_interface-${ERL_INTERFACE_VER}/include -DLINUX

LDFLAGS=	-L/opt/erlang/lib/erlang/lib/erl_interface-${ERL_INTERFACE_VER}/lib \
		-lerl_interface -lei -lpthread

ping:	ping.c
	$(CC) $(CFLAGS) ping.c -o ping $(LDFLAGS)

clean:
	rm -f *~ ping ping.o

