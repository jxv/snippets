#ifndef COMMON_H
#define COMMON_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <SDL/SDL_net.h>

typedef enum
{
	msgAck = 0,
	msgText,
	msgQuit,
	msgCount
} MsgType;

typedef struct
{
	MsgType type;
	union {
		char text[256];
	} data;
} Msg;

#define MSG_QUEUE_MAX 10

typedef struct
{
	Msg msg[MSG_QUEUE_MAX];
	int head;
	int tail;
	bool full;
	bool writing;
	bool reading;
} MsgQueue;

typedef struct
{
	bool quit;
	TCPsocket sd;
	IPaddress *ip;
	SDL_Thread *commThd;
	SDL_Thread *timeThd;
	SDL_Thread *inputThd;
	SDL_Thread *killThd;
	int timeCount;
	Msg msgSend;
	Msg msgRecv;
	bool read;
} ConnInfo;

int getLine(char *line, size_t s);

void sendTcp(TCPsocket sd, void *data, size_t s);

int timeFunc(void *data);
int inputFunc(void *data);
int killFunc(void *data);


#endif

