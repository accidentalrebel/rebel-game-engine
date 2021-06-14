#include "text.h"
#include <stdlib.h>
#include "sprite.h"

Text* TextCreate(char* string) {
	Text* text = (Text*)malloc(sizeof(Text));
	text->canvas = SpriteCreate(500, 500);
	text->string = string;
	return text;
}

Font* FontLoad(const char* directory, const char* filename, char* typeName) {
	Font* font = (Font*)malloc(sizeof(Font));
	font->fontTexture = TextureLoad(directory, filename, typeName);
	return font;
}

void FontUnload(Font* font) {
	TextureUnload(font->fontTexture);
	free(font);
}
