#pragma once

#include "../external/cglm/cglm.h"

// TODO: Should accept multiple textures
typedef struct Material
{
	unsigned int textureDiffuse1;
	unsigned int textureSpecular1;
	float shininess;
	vec3 color;
} Material;

Material* MaterialCreate();
