#pragma once

#include "../../data/structs.h"
#include "../../data/vec3.h"

typedef struct Light
{
	Vec3* ambient;
	Vec3* diffuse;
	Vec3* specular;
} Light;

typedef struct DirectionLight
{
	Vec3* direction;
	Light* light;
} DirectionLight;

typedef struct PointLight
{
	Vec3* position;
	Light* light;

	float constant;
	float linear;
	float quadratic;
} PointLight;

Light* LightCreate();
DirectionLight* DirectionLightCreate(Vec3* direction, Vec3* ambient, Vec3* diffuse, Vec3* specular);
PointLight* PointLightCreate(Vec3* position, Vec3* ambient, Vec3* diffuse, Vec3* specular, float constant, float linear, float quadratic);
