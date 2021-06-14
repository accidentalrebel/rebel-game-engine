#pragma once

#include "texture.h"

typedef struct Font {
	Texture* fontTexture;
} Font;

Font* FontLoad(const char* directory, const char* filename, char* typeName);
void FontUnload(Font* font);
