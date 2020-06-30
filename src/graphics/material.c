#include "material.h"
#include <stdlib.h>
#include <string.h>
#include "shader.h"

#define MAX_TEXTURES_COUNT 10

Material* MaterialCreate()
{
	Material* mat = (Material*)malloc(sizeof(Material));
	mat->textureDiffuse1 = 0;
	mat->textureSpecular1 = 0;
	mat->shininess = 1.0f;
	mat->loadedTextures = (Texture**)malloc(MAX_TEXTURES_COUNT * sizeof(Texture));

	glm_vec3_zero(mat->color);
	
	return mat;
}

void MaterialAddTexture(Material* material, Texture* texture)
{
	material->loadedTextures[material->loadedTexturesCount] = texture;
}

Texture* MaterialLoadTexture(const char* directory, const char* fileName, char* typeName)
{
	Texture* texture = (Texture*)malloc(sizeof(Texture));
	char fullPath[100] = "";
	strcat(fullPath, directory);
	strcat(fullPath, fileName);

	texture->id = TextureLoad(fullPath);
	texture->type = typeName;
	texture->path = malloc(strlen(fileName)+1);
	strcpy(texture->path, fileName);

	return texture;
}
