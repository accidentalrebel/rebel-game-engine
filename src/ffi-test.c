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

Vector3* MakeVector3(float x, float y, float z)
{
	Vector3* v = (Vector3*)malloc(sizeof(Vector3));
	v->x = x;
	v->y = y;
	v->z = z;
	return v;
}
