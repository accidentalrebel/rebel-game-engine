#pragma once
#include "mesh.h"
#include "model.h"
#include "material.h"
#include "../external/cglm/cglm.h"

void RendererDraw(Model* modelObject, vec3 position, vec3 scale, vec3 rotation, vec4 color);
