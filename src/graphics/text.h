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
	FontChar** fontChars;
	unsigned short fontCharsSize;
	unsigned short* charMap;
	unsigned short baseHeight;
} Font;

typedef struct Text {
	Font* font;
	Model* canvas;
	char* string;
	unsigned int textWidth;
	unsigned int textHeight;
} Text;

Text* TextCreate(char* string);
void TextDestroy(Text* text);
void TextLoadFont(Text* text, Font *font);

void TextGetWidthAndHeight(Text* text);
	
Font* FontLoad(const char* directory, const char* filename, char* typeName);
void FontUnload(Font* font);

FontChar* GetFontChar(Font* font, unsigned short id);
