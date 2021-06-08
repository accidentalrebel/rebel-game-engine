#include "material.h"
#include "shader.h"

#define MAX_TEXTURES_COUNT 10

Material* MaterialCreate()
{
	Material* mat = (Material*)malloc(sizeof(Material));
	mat->textureDiffuse1 = 0;
	mat->textureSpecular1 = 0;
	mat->shininess = 1.0f;
	mat->loadedTextures = (Texture**)malloc(MAX_TEXTURES_COUNT * sizeof(Texture));
	mat->loadedTexturesCount = 0;

	glm_vec3_zero(mat->color);
	
	return mat;
}
