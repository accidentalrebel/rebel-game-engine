#pragma once

#include "shader.h"
#include <glm/glm.hpp>
#include <glm/gtc/matrix_transform.hpp>

namespace rebel
{
	class Sprite
	{
	 public:
		void initialize(Shader *shader, const char *directory, const char *filename);
		void draw(glm::vec2 position, float width, float height, glm::vec3 tintColor = glm::vec3(1.0f));
	 private:
		unsigned int VAO;
		unsigned int VBO;
		unsigned int texture;
		Shader *shader;
	};
}
