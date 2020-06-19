#include "light.h"
#include "../../rebel.h"
#include <stdlib.h>

const float POINT_LIGHT_CONSTANT = 1.0f;
const float POINT_LIGHT_LINEAR = 0.14f;
const float POINT_LIGHT_QUADRATIC = 0.07f;

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

PointLight* PointLightCreate2(vec3 position, vec3 color)
{
	PointLight* pointLight = (PointLight*)malloc(sizeof(PointLight));
	glm_vec3_copy(position, pointLight->position);
	pointLight->constant = POINT_LIGHT_CONSTANT;
	pointLight->linear = POINT_LIGHT_LINEAR;
	pointLight->quadratic = POINT_LIGHT_QUADRATIC;

	pointLight->light = LightCreate((vec3) { 0.1f, 0.1f, 0.1f },
																	color,
																	(vec3){ 0, 0, 0 });

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
