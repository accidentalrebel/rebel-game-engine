#pragma once
#include <glad/glad.h>
#include <GLFW/glfw3.h>

namespace rebel
{
	class Window
	{
	 public:
		bool initialize(int windowWidth, int windowHeight, const char* windowName);
		bool canClose();
	 private:
		GLFWwindow* glWindow;
	};
}
