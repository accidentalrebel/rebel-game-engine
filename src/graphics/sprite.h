#pragma once

#include "../rebel.h"
#include "../data/structs.h"
#include "shader.h"
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>

Sprite* SpriteCreate(const char *directory, const char *filename);
void SpriteDraw(Sprite *sprite, Vec3 *position, float width, float height, Vec3 *tintColor, Shader* shader);
