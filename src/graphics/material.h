#pragma once

#include "../external/cglm/cglm.h"
#include "mesh.h"

typedef struct Material
{
	unsigned int textureDiffuse1;
	unsigned int textureSpecular1;

	Texture** loadedTextures;
	unsigned int loadedTexturesIndex;
	
	float shininess;
	vec3 color;
} Material;

Material* MaterialCreate();
