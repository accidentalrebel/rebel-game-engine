#pragma once

#include "../../data/structs.h"
#include "../../data/vec3.h"

Light* LightCreate();
DirectionLight* DirectionLightCreate(Vec3* direction, Vec3* ambient, Vec3* diffuse, Vec3* specular);
