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
		void clear();
		void destroy();
	 private:
		GLFWwindow* glWindow;
		
		static void framebuffer_size_callback(GLFWwindow* window, int width, int height);
	};
}
