all: eram

debug: eram.c
	$(CC) eram.c -Wall -Wextra -Werror -Wpedantic -DDEBUG -o eram_dbg

eram: eram.c
	$(CC) eram.c -Wall -Wextra -Werror -Wpedantic -DNDEBUG -o eram

clean:
	rm eram
