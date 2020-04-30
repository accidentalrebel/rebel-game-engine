#include "window.h"

bool rebel::Window::initialize(int windowWidth, int windowHeight, const char* windowName)
{
	glfwInit();
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
	//glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE); // Uncomment for MacOSX

	glWindow = glfwCreateWindow(windowWidth, windowHeight, windowName, NULL, NULL);
	if (glWindow == NULL)
	{
    glfwTerminate();
		return false;
	}
	glfwMakeContextCurrent(glWindow);

	return true;
}

bool rebel::Window::canClose()
{
	glfwSwapBuffers(glWindow);
	glfwPollEvents();  
	
	return glfwWindowShouldClose(glWindow);
}
