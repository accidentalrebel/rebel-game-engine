#pragma once

#include "../external/cglm/cglm.h"
#include "mesh.h"

typedef struct Texture {
	unsigned int id;
	char* type;
	char* path;
} Texture;

typedef struct Material
{
	unsigned int textureDiffuse1;
	unsigned int textureSpecular1;

	Texture** loadedTextures;
	unsigned int loadedTexturesCount;
	
	float shininess;
	vec3 color;
} Material;

Material* MaterialCreate();
void MaterialAddTexture(Material* material, Texture* texture);
Texture* MaterialLoadTexture(const char* directory, const char* fileName, char* typeName);
