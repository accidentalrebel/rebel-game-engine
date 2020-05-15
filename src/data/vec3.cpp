#include "vec3.hpp"

glm::vec3 Vec3ToGlm(Vec3 vec)
{
	return glm::vec3(vec.x, vec.y, vec.z);
}

void Vec3FromGlm(Vec3* to, glm::vec3 from)
{
	to->x = from.x;
	to->y = from.y;
	to->z = from.z;
}
