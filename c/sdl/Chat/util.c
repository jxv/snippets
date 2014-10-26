#include "common.h"

int getLine(char *line, size_t s)
{
	int c, len;

	line = fgets(line, s, stdin);
	line[strcspn(line, "\n")] = '\0';
	len = strlen(line);
	if (len == s - 1) {
		while ((c = getchar()) != '\n' && c != EOF);
	}
	return len;
}

void sendTcp(TCPsocket sd, void *data, size_t s)
{
	if (SDLNet_TCP_Send(sd, data, s) != s) {
		fprintf(stderr, "SDLNet_TCP_Send: %s\n", SDLNet_GetError());
		exit(EXIT_FAILURE);
	}
}

int timeFunc(void *data)
{
	const int timeout = 10;  // secs
	ConnInfo *ci = data;

	ci->timeCount = 0;
	while (!ci->quit) {
		if (ci->timeCount > timeout) {
			printf("timeout\n");
			ci->quit = true;
		}
		SDL_Delay(1000);
		ci->timeCount++;
	}
	return 0;
}

int inputFunc(void *data)
{
	ConnInfo *ci = data;
	
	while (!ci->quit) {
		while (ci->read);
		printf("> ");
		getLine(ci->msgSend.data.text, 256);
		if (strcmp(ci->msgSend.data.text, "quit") == 0) {
			ci->msgSend.type = msgQuit;
		} else {
			ci->msgSend.type = msgText;
		}
		ci->read = true;
	}
	return 0;	
}

int killFunc(void *data)
{
	ConnInfo *ci = data;

	while (!ci->quit);
	SDL_KillThread(ci->timeThd);
	SDL_KillThread(ci->commThd);
	SDL_KillThread(ci->inputThd);

	SDLNet_TCP_Close(ci->sd);

	return 0;
}

