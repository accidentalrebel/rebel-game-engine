#pragma once
#include <glad/glad.h>
#include <GLFW/glfw3.h>

namespace rebel
{
	class Window
	{
	 public:
		Window(int windowWidth, int windowHeight);
		bool canCloseWindow();
	 private:
		GLFWwindow* window;
	};
}
