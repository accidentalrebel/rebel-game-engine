#include "vec3.hpp"

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
