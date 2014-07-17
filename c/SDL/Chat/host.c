#include "common.h"

int commFunc(void *data)
{
	ConnInfo *ci = data;

	while (!ci->quit) {

		// Recv msg

		if (SDLNet_TCP_Recv(ci->sd, &ci->msgRecv.type, sizeof(ci->msgRecv.type)) <= 0) {
			continue;
		}

		ci->timeCount = 0;
		switch (ci->msgRecv.type) {
		case msgAck: break;
		case msgText: {
			if (SDLNet_TCP_Recv(ci->sd, &ci->msgRecv, sizeof(ci->msgRecv)) > 0) {
				printf("recv: [%s]\n", ci->msgRecv.data.text);
			}
			break;
		}
		case msgQuit: {
			ci->quit = true;
			printf("Host quit: %x %d\n",
			      	SDLNet_Read32(&ci->ip->host),
				SDLNet_Read16(&ci->ip->port));
			break;
	   	}
		default: break;
		}
		ci->timeCount = 0;
		if (ci->msgRecv.type == msgQuit) {
			ci->quit = true;
			break;
		}

		// Send msg
		
		sendTcp(ci->sd, &ci->msgSend.type, sizeof(ci->msgSend.type));

		if (ci->read) {
			sendTcp(ci->sd, &ci->msgSend, sizeof(ci->msgSend));
			ci->msgSend.type = msgAck;
			ci->read = false;
		}
		ci->timeCount = 0;
	}		
	return 0;	
}

void initConn(ConnInfo *ci)
{
	ci->quit = false;
	ci->read = false;
	ci->timeCount = 0;
	ci->msgSend.type = msgAck;
	ci->msgRecv.type = msgAck;

	ci->commThd = SDL_CreateThread(commFunc, ci);
	ci->timeThd = SDL_CreateThread(timeFunc, ci);
	ci->inputThd = SDL_CreateThread(inputFunc, ci);
	ci->killThd = SDL_CreateThread(killFunc, ci);
}

typedef struct
{
	ConnInfo ci[4];
	SDL_Thread *inputThd;
} ConnsInfo;

int main(int argc, char **argv)
{
	TCPsocket sd; /* Socket descriptor, Client socket descriptor */
	IPaddress ip;
	bool quit;
	ConnInfo ci;
 
	{
		if (SDLNet_Init() < 0) {
			fprintf(stderr, "SDLNet_Init: %s\n", SDLNet_GetError());
			exit(EXIT_FAILURE);
		}
		if (SDLNet_ResolveHost(&ip, NULL, 2000) < 0) {
			fprintf(stderr, "SDLNet_ResolveHost: %s\n", SDLNet_GetError());
			exit(EXIT_FAILURE);
		}
		if (!(sd = SDLNet_TCP_Open(&ip))) {
			fprintf(stderr, "SDLNet_TCP_Open: %s\n", SDLNet_GetError());
			exit(EXIT_FAILURE);
		}
	}
	
	quit = false;
	while (!quit) {
		if ((ci.sd = SDLNet_TCP_Accept(sd))) {
			if ((ci.ip = SDLNet_TCP_GetPeerAddress(ci.sd))) {
				printf("Host connected: %x %d\n",
					SDLNet_Read32(&ci.ip->host),
					SDLNet_Read16(&ci.ip->port));
			} else {
				fprintf(stderr, "SDLNet_TCP_GetPeerAddress: %s\n", SDLNet_GetError());
 			}

			initConn(&ci);
			while (!ci.quit);
			SDL_Delay(2000);
		}
		quit = ci.quit;
	}
 
	SDLNet_TCP_Close(sd);
	SDLNet_Quit();
 
	return EXIT_SUCCESS;
}

