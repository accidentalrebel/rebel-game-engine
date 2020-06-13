#include "ffi-test.h"
#include <stdio.h>
#include <stdlib.h>

void FFITest()
{
	printf("#### FFITEST\n");
}

void StringAccept(char* v)
{
	printf("### %s\n", v);
}

void Vector3Accept(Vector3 v)
{
	printf("#### %f, %f, %f\n", v.x, v.y, v.z);
}

void SetCameraProjection(int projection)
{
	printf("## Camera projection: %i", projection);
}
