#pragma once

#include "texture.h"
#include "model.h"

typedef struct FontChar {
	int id;
	int width;
	int height;
} FontChar;

typedef struct Font {
	Texture* fontTexture;
	FontChar** fontChar;
} Font;

typedef struct Text {
	Font* font;
	Model* canvas;
	char* string;
} Text;

Text* TextCreate(char* string);
void TextLoadFont(Text* text, Font *font);
	
Font* FontLoad(const char* directory, const char* filename, char* typeName);
void FontUnload(Font* font);


