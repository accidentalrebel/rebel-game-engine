#pragma once
#include "mesh.h"
#include "model.h"
#include "material.h"
#include "../external/cglm/cglm.h"

Model* SpriteCreate(float width, float height);
void RendererDraw(Model* modelObject, vec3 position, vec3 color);
