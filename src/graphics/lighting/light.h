#pragma once

#include "../../external/cglm/cglm.h"

typedef struct Light
{
	vec3 ambient;
	vec3 diffuse;
	vec3 specular;
} Light;

typedef struct DirectionLight
{
	vec3 direction;
	Light* light;
} DirectionLight;

typedef struct PointLight
{
	vec3 position;
	Light* light;

	float constant;
	float linear;
	float quadratic;
} PointLight;


Light* LightCreate(vec3 ambient, vec3 diffuse, vec3 specular);
DirectionLight* DirectionLightCreate(vec3 direction, vec3 ambient, vec3 diffuse, vec3 specular);
PointLight* PointLightCreate(vec3 position, vec3 ambient, vec3 diffuse, vec3 specular, float constant, float linear, float quadratic);
