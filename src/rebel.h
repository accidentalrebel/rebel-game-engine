#pragma once
#include "core/window.h"
#include "graphics/sprite.h"

namespace rebel
{
	class Rebel
	{
	 public:
		bool initialize(unsigned int windowWidth, unsigned int windowHeight, const char* windowName);
		bool canClose();
		
		void processInput();
		void destroy();

		Window* window;
	};
}

