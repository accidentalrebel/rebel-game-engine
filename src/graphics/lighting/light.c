#include "light.h"
#include "../../rebel.h"
#include <stdlib.h>

Light* LightCreate(Vec3* ambient, Vec3* diffuse, Vec3* specular)
{
	Light* light = (Light*)malloc(sizeof(Light));
	glm_vec3_copy((vec3){ ambient->x, ambient->y, ambient->z }, light->ambient);
	glm_vec3_copy((vec3){ diffuse->x, diffuse->y, diffuse->z }, light->diffuse);
	glm_vec3_copy((vec3){ specular->x, specular->y, specular->z }, light->specular);

	return light;
}

PointLight* PointLightCreate(Vec3* position, Vec3* ambient, Vec3* diffuse, Vec3* specular, float constant, float linear, float quadratic)
{
	PointLight* pointLight = (PointLight*)malloc(sizeof(PointLight));
	glm_vec3_copy((vec3){position->x, position->y, position->z}, pointLight->position);
	pointLight->constant = constant;
	pointLight->linear = linear;
	pointLight->quadratic = quadratic;
	
	pointLight->light = LightCreate(ambient, diffuse, specular);

	g_rebel.pointLights[g_rebel.pointLightCount++] = pointLight;
	
	return pointLight;
}

DirectionLight* DirectionLightCreate(Vec3* direction, Vec3* ambient, Vec3* diffuse, Vec3* specular)
{
	if ( g_rebel.directionLight != NULL ) 
		printf("WARNING::DIRECTION_LIGHT::THERE IS AN ALREADY EXISTING DIRECTION LIGHT. ENGINE CAN ONLY SUPPORT ONE (FOR NOW).\n");
	
	DirectionLight* directionLight = (DirectionLight*)malloc(sizeof(DirectionLight));

	// We make a new Vector3 and copied the values from the Vec3's passed from the script
	// This is so that those pointers can be safely cleaned up by the GC without affecting these values
	glm_vec3_copy((vec3){direction->x, direction->y, direction->z}, directionLight->direction);

	directionLight->light = LightCreate(ambient, diffuse, specular);

	g_rebel.directionLight = directionLight;
	return directionLight;
}
