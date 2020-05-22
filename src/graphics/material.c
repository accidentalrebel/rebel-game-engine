#include "material.h"
#include <stdlib.h>
#include "../data/vec3.h"

Material* MaterialCreate(Vec3 *ambient, Vec3 *diffuse, Vec3* specular)
{
	Material* mat = (Material*)malloc(sizeof(Material));
	mat->ambient = Vec3Copy(ambient);
	mat->diffuse = Vec3Copy(diffuse);
	mat->specular = Vec3Copy(specular);
	return mat;
}
