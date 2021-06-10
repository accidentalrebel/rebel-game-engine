#pragma once

typedef struct Texture {
	unsigned int id;
	char* type;
	char* path;
	int width;
	int height;
} Texture;

Texture* TextureLoad(const char* directory, const char* fileName, char* typeName);
void TextureUnload();