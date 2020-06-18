#pragma once

#include "../external/cglm/cglm.h"

typedef struct Vec3
{
	float x;
	float y;
	float z;
} Vec3;


Vec3* Vec3Create(float x, float y, float z);
