#include "material.h"
#include <stdlib.h>

Material* MaterialCreate()
{
	Material* mat = (Material*)malloc(sizeof(Material));
	mat->textureDiffuse1 = 0;
	mat->textureSpecular1 = 0;
	mat->shininess = 1.0f;

	glm_vec3_zero(mat->color);
	
	return mat;
}
