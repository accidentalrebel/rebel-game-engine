#pragma once

#include "../../data/structs.h"
#include "../../data/vec3.h"

Light* LightCreate(Vec3* color);
void LightSetColor(Light* light, Vec3* color);
