#pragma once

#include "texture.h"
#include "model.h"

typedef struct FontChar {
	unsigned short id;
	unsigned short x;
	unsigned short y;
	unsigned short width;
	unsigned short height;
	unsigned short xAdvance;
	unsigned short xOffset;
	unsigned short yOffset;
} FontChar;

typedef struct Font {
	char* fontPath;
	Texture* fontTexture;
	FontChar** fontChar;
	unsigned short* charMap;
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

FontChar* GetFontChar(Font* font, unsigned short id);


