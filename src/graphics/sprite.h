#pragma once

#include "shader.h"
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>

typedef struct LSprite
{
	unsigned int VAO;
	unsigned int VBO;
	unsigned int texture;
} LSprite;

LSprite CreateSprite(const char *directory, const char *filename);
void DrawSprite(LSprite *sprite, float width, float height);
