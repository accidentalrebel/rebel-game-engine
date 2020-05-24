#include "direction_light.h"
#include "light.h"
#include "../../rebel.h"
#include <stdlib.h>

DirectionLight* DirectionLightCreate(Vec3* direction, Vec3* color)
{
	if ( g_rebel.directionLight != NULL ) 
		printf("WARNING::DIRECTION_LIGHT::THERE IS AN ALREADY EXISTING DIRECTION LIGHT. ENGINE CAN ONLY SUPPORT ONE (FOR NOW).\n");
	
	DirectionLight* directionLight = (DirectionLight*)malloc(sizeof(DirectionLight));

	// We make a new Vector3 and copied the values from the Vec3's passed from the script
	// This is so that those pointers can be safely cleaned up by the GC without affecting these values
	directionLight->direction = Vec3Create(direction->x, direction->y, direction->z);

	directionLight->light = LightCreate(color);

	g_rebel.directionLight = directionLight;
	return directionLight;
}
