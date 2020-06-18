#include "light.h"
#include "../../rebel.h"
#include <stdlib.h>

Light* LightCreate(vec3 ambient, vec3 diffuse, vec3 specular)
{
	Light* light = (Light*)malloc(sizeof(Light));
	glm_vec3_copy(ambient, light->ambient);
	glm_vec3_copy(diffuse, light->diffuse);
	glm_vec3_copy(specular, light->specular);

	return light;
}

PointLight* PointLightCreate(vec3 position, vec3 ambient, vec3 diffuse, vec3 specular, float constant, float linear, float quadratic)
{
	PointLight* pointLight = (PointLight*)malloc(sizeof(PointLight));
	glm_vec3_copy(position, pointLight->position);
	pointLight->constant = constant;
	pointLight->linear = linear;
	pointLight->quadratic = quadratic;
	
	pointLight->light = LightCreate(ambient, diffuse, specular);

	g_rebel.pointLights[g_rebel.pointLightCount++] = pointLight;
	
	return pointLight;
}

DirectionLight* DirectionLightCreate(vec3 direction, vec3 ambient, vec3 diffuse, vec3 specular)
{
	if ( g_rebel.directionLight != NULL ) 
		printf("WARNING::DIRECTION_LIGHT::THERE IS AN ALREADY EXISTING DIRECTION LIGHT. ENGINE CAN ONLY SUPPORT ONE (FOR NOW).\n");
	
	DirectionLight* directionLight = (DirectionLight*)malloc(sizeof(DirectionLight));

	glm_vec3_copy(direction, directionLight->direction);

	directionLight->light = LightCreate(ambient, diffuse, specular);

	g_rebel.directionLight = directionLight;
	return directionLight;
}
