#pragma once

#include "shader.h"

namespace rebel
{
	class Sprite
	{
	 public:
		void initialize();
		void draw();
	 private:
		unsigned int VAO;
		unsigned int VBO;
		unsigned int texture;
		Shader *shader;
	};
}
