#include "texture.h"
#include <stdlib.h>
#include <string.h>
#include "shader.h"

Texture* TextureLoad(const char* directory, const char* fileName, char* typeName)
{
	Texture* texture = (Texture*)malloc(sizeof(Texture));
	char fullPath[100] = "";
	strcat(fullPath, directory);
	strcat(fullPath, fileName);

	texture->id = ShaderTextureLoad(fullPath);
	texture->type = typeName;
	texture->path = malloc(strlen(fileName)+1);
	strcpy(texture->path, fileName);

	return texture;
}
