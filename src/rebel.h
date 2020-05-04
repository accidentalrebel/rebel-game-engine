#pragma once

typedef struct Vec3 {
    float x;
    float y;
    float z;
} Vec3;

#include "core/window.h"
#include "graphics/sprite.h"
#include "graphics/shader.h"

extern Shader *g_defaultShader;

namespace rebel
{
	class Rebel
	{
	 public:
		static Rebel* initialize(unsigned int windowWidth, unsigned int windowHeight, const char* windowName);
		
		bool canClose();
		void processInput();
		void destroy();

		Window* window;
		static Rebel *instance;
	 private:
		Rebel(void) {}
	};
}
