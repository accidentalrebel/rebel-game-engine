#include "light.h"
#include <stdlib.h>

Light* LightCreate(Vec3* color)
{
	Light* light = (Light*)malloc(sizeof(Light));
	LightSetColor(light, color);
	return light;
}
void LightSetColor(Light* light, Vec3* color)
{
	light->ambient = Vec3Copy(color);
	light->diffuse = Vec3Create(1.0f, 1.0f, 1.0f);
	light->specular = Vec3Create(1.0f, 1.0f, 1.0f);
}
