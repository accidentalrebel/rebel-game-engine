#include "vec3.h"

Vec3* Vec3Create(float x, float y, float z)
{
	Vec3* v = (Vec3*)malloc(sizeof(Vec3));
	v->x = x;
	v->y = y;
	v->z = z;
	return v;
}

Vec3* Vec3Copy(Vec3* from)
{
	return Vec3Create(from->x, from->y, from->z);
}

void Vec3ToGlm(Vec3* vec, vec3 to)
{
	glm_vec3_zero(to);
	to[0] = vec->x;
	to[1] = vec->y;
	to[2] = vec->z;
}

void Vec3FromGlm(Vec3* to, vec3 from)
{
	to->x = from[0];
	to->y = from[1];
	to->z = from[2];
}
