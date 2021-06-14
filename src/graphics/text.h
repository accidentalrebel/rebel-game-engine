#pragma once

#include "texture.h"
#include "model.h"

typedef struct Font {
	Texture* fontTexture;
} Font;

typedef struct Text {
	Font* font;
	Model* canvas;
	char* string;
} Text;

Text* TextCreate(char* string);
Font* FontLoad(const char* directory, const char* filename, char* typeName);
void FontUnload(Font* font);


