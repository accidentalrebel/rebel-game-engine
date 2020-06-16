#pragma once

#include "../data/structs.h"
#include "../external/cglm/cglm.h"

typedef struct Material
{
	unsigned int textureDiffuse1;
	unsigned int textureSpecular1;
	float shininess;
	vec3 color;
} Material;

Material* MaterialCreate();
