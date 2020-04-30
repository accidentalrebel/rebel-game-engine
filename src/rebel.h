#pragma once
#include "core/window.h"

namespace rebel
{
	class Rebel
	{
	 public:
		bool initialize();
		bool canClose();

		Window* window;
	};
}
