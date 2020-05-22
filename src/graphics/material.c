#include "material.h"
#include <stdlib.h>
#include "../data/vec3.h"

Material* MaterialCreate()
{
	Material* mat = (Material*)malloc(sizeof(Material));
	mat->ambient = Vec3Create(1.0, 1.0, 1.0);
	mat->diffuse = Vec3Create(0.0, 0.0, 0.0);
	mat->specular = Vec3Create(0.0, 0.0, 0.0);
	mat->shininess = 1.0f;
	return mat;
}
