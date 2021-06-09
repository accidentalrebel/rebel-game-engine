#pragma once
#include "model.h"

Model* SpriteCreate(float width, float height);
void SpriteAddTexture(Model* sprite, Texture* texture);
