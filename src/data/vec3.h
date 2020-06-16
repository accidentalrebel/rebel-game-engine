#pragma once

#include "structs.h"
#include "../external/cglm/cglm.h"

Vec3* Vec3Create(float x, float y, float z);
Vec3* Vec3Copy(Vec3* from);
void Vec3Set(Vec3* vec, float x, float y, float z);
void Vec3ToGlm(Vec3* vec, vec3 to);
void Vec3FromGlm(Vec3* to, vec3 vec);
