#include "material.h"
#include <stdlib.h>
#include "../data/vec3.h"

Material* MaterialCreate()
{
	Material* mat = (Material*)malloc(sizeof(Material));
	mat->specular = Vec3Create(0.0f, 0.0f, 0.0f);
	mat->shininess = 1.0f;
	return mat;
}
