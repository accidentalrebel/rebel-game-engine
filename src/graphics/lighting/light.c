#include "light.h"
#include <stdlib.h>

Light* LightCreate()
{
	Light* light = (Light*)malloc(sizeof(Light));
	return light;
}
