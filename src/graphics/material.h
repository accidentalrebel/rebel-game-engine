#pragma once

#include "../external/cglm/cglm.h"
#include "mesh.h"
#include "texture.h"

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
