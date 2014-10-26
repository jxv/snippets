#include <SDL/SDL.h>

typedef enum
{
	false = 0,
	true = 1
} bool;	

SDL_Surface *scr = NULL;

void init()
{
	if (SDL_Init(SDL_INIT_EVERYTHING) == -1)
		exit(1);
	SDL_ShowCursor(SDL_DISABLE);
	SDL_WM_SetCaption("basic", NULL);
	scr = SDL_SetVideoMode(640, 480, 32, SDL_SWSURFACE);
	if (scr == NULL)
		exit(1);
}

void exec()
{
	bool q;
	SDL_Event e;
	
	q = false;
	do {
		while(SDL_PollEvent(&e)) {
			switch(e.type) {
				case SDL_QUIT:
					q = true;
					break;
				case SDL_KEYDOWN:
					if (e.key.keysym.sym == SDLK_ESCAPE) 
						q = true; 
					break;
				default: break;
			}
		}
		SDL_FillRect(scr, NULL, 0x00000000);
		SDL_Flip(scr);
		SDL_Delay(0);
	} while (q == false);
}

void quit()
{
	SDL_FreeSurface(scr);
	SDL_Quit();
}

int main()
{ 
	init();
	exec();
	quit();
	return 0;
}

