#include "material.h"
#include <stdlib.h>
#include "../data/vec3.h"

Material* MaterialCreate()
{
	Material* mat = (Material*)malloc(sizeof(Material));
	mat->textureDiffuse1 = 0;
	mat->textureSpecular1 = 0;
	mat->shininess = 1.0f;
	mat->color = NULL;
	return mat;
}
