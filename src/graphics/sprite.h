#pragma once
#include "model.h"

Model* SpriteCreate(float width, float height);
void SpriteLoadTexture(Model* sprite, Texture* texture);
void SpriteUnloadTexture();

