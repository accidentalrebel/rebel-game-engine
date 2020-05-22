#pragma once

#include "structs.h"
#include <cglm/cglm.h>

Vec3* Vec3Create(float x, float y, float z);
Vec3* Vec3Copy(Vec3* from);
void Vec3ToGlm(Vec3* vec, vec3 to);
void Vec3FromGlm(Vec3* to, vec3 vec);
