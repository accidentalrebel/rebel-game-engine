#include "stub.h"
#include <cglm/cglm.h>
#include "rebel.h"

void RebelTest_(Renderer* r, float v1, float v2, float v3, float x, float y)
{
	RebelTest(r, (float[3]){ v1, v2, v3 }, x, y);
}

