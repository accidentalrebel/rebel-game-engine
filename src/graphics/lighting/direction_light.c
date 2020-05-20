#include "direction_light.h"
#include "../../rebel.h"
#include <stdlib.h>

DirectionLight* DirectionLightCreate(Vec3* position, Vec3* direction, Vec3* color)
{
	if ( g_rebel.directionLight != NULL ) 
		printf("WARNING::DIRECTION_LIGHT::THERE IS AN ALREADY EXISTING DIRECTION LIGHT. ENGINE CAN ONLY SUPPORT ONE (FOR NOW).\n");
	
	DirectionLight* directionLight = (DirectionLight*)malloc(sizeof(DirectionLight));
	directionLight->position = position;
	directionLight->direction = direction;
	directionLight->color = color;

	/* directionLight->lightIntensity->ambient = 1.0f; */
	/* directionLight->lightIntensity->diffuse = 1.0f; */
	/* directionLight->lightIntensity->specular = 1.0f; */

	g_rebel.directionLight = directionLight;
	return directionLight;
}
