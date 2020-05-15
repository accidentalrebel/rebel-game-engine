#pragma once

#include "structs.h"
#include "../external/glm/glm.hpp"

glm::vec3 Vec3ToGlm(Vec3 vec);
void Vec3FromGlm(Vec3* to, glm::vec3 vec);
