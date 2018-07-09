#include "SDL2/SDL_ttf.h"

//SDL_Surface *TTF_RenderUTF8_Blended(TTF_Font *font, const char *text, SDL_Color fg)
SDL_Surface* wrapper_TTF_RenderUTF8_Blended(TTF_Font *font, const char *text,
		unsigned char r, unsigned char g, unsigned char b, unsigned char a)
{
	SDL_Color color;
	color.r = r; color.g = g; color.b = b; color.a = a;
	return TTF_RenderUTF8_Blended(font, text, color);
}
