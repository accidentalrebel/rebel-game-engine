#pragma once
#include "../data/vec3.h"
#include "../../data/structs.h"	

DirectionLight* DirectionLightCreate(Vec3* direction, Vec3* ambient, Vec3* diffuse, Vec3* specular);
