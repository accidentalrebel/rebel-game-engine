#pragma once

#include "shader.h"
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>

typedef struct Sprite
{
	unsigned int VAO;
	unsigned int VBO;
	unsigned int texture;
} Sprite;

Sprite CreateSprite(const char *directory, const char *filename);
void DrawSprite(Sprite *sprite, float width, float height);
