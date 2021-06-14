#include "text.h"
#include <stdlib.h>

Font* FontLoad(const char* directory, const char* filename, char* typeName) {
	Font* font = (Font*)malloc(sizeof(Font));
	font->fontTexture = TextureLoad(directory, filename, typeName);
	return font;
}

void FontUnload(Font* font) {
	TextureUnload(font->fontTexture);
	free(font);
}
