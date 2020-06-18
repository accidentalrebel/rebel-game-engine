#include "vec3.h"

Vec3* Vec3Create(float x, float y, float z)
{
	Vec3* v = (Vec3*)malloc(sizeof(Vec3));
	v->x = x;
	v->y = y;
	v->z = z;
	return v;
}
