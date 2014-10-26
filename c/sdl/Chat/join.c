#include "common.h"

int commFuncJoin(void *data)
{
	ConnInfo *ci = data;
	ci->quit = false;
	
	while (!ci->quit) {

		// Send msg

		sendTcp(ci->sd, (void*)&ci->msgSend.type, sizeof(ci->msgSend.type));
		
		switch (ci->msgSend.type) {
		case msgAck: break;
		case msgText: {
			sendTcp(ci->sd, (void*)&ci->msgSend, sizeof(ci->msgSend));
			ci->msgSend.type = msgAck; 
			break;
		}
		case msgQuit: break;
		default: break;
		}
		if (ci->msgSend.type == msgQuit) {
			ci->quit = true;
			break;
		}

		ci->read = false;

		// Recv msg
		
		if (SDLNet_TCP_Recv(ci->sd, (void*)&ci->msgRecv.type, sizeof(ci->msgRecv.type)) > 0) { 
			switch (ci->msgRecv.type) {
			case msgAck: break;
			case msgText: {
				if (SDLNet_TCP_Recv(ci->sd, (void*)&ci->msgRecv, sizeof(ci->msgRecv)) > 0) { 
					printf("recv: [%s]\n", ci->msgRecv.data.text);
				}
				break;
			}
			case msgQuit: ci->quit = true; break;
			default: break;
			}		
		}
	}

	return 0;
}

int main(int argc, char **argv)
{
	IPaddress ip;
	ConnInfo ci;
 
	{	// check for errors
		if (argc < 3) {
			fprintf(stderr, "Usage: %s host port\n", argv[0]);
			exit(EXIT_FAILURE);
		}
		if (SDLNet_Init() < 0) {
			fprintf(stderr, "SDLNet_Init: %s\n", SDLNet_GetError());
			exit(EXIT_FAILURE);
		}
		if (SDLNet_ResolveHost(&ip, argv[1], atoi(argv[2])) < 0) {
			fprintf(stderr, "SDLNet_ResolveHost: %s\n", SDLNet_GetError());
			exit(EXIT_FAILURE);
		}
		if (!(ci.sd = SDLNet_TCP_Open(&ip))) {
			fprintf(stderr, "SDLNet_TCP_Open: %s\n", SDLNet_GetError());
			exit(EXIT_FAILURE);
		}
	}
	

	ci.ip = &ip;

	ci.quit = false;
	ci.read = false;
	ci.timeCount = 0;
	ci.msgSend.type = msgAck;
	ci.msgRecv.type = msgAck;

	ci.timeThd = SDL_CreateThread(timeFunc, &ci);
	ci.commThd = SDL_CreateThread(commFuncJoin, &ci);
	ci.inputThd = SDL_CreateThread(inputFunc, &ci);
	while (!ci.quit);
	SDL_KillThread(ci.timeThd);
	SDL_KillThread(ci.commThd);
	SDL_KillThread(ci.inputThd);

	SDLNet_TCP_Close(ci.sd);
	SDLNet_Quit();
 
	return EXIT_SUCCESS;
}

