#pragma once

#include "../rebel.h"
#include "shader.h"
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>

typedef struct Sprite
{
	unsigned int VAO;
	unsigned int VBO;
	unsigned int texture;
} Sprite;

namespace sprite
{
	Sprite Create(const char *directory, const char *filename);
	void Draw(Sprite *sprite, Vec3 position, float width, float height, Vec3 tintColor);
}
