#include "window.h"
#include <iostream>

using namespace rebel;

bool Window::initialize(int windowWidth, int windowHeight, const char* windowName)
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
	glfwSetFramebufferSizeCallback(glWindow, framebuffer_size_callback);
	
	if ( !gladLoadGLLoader((GLADloadproc)glfwGetProcAddress))
	{
		std::cout << "Failed to initialize GLAD" << std::endl;
		return -1;
	}

	glEnable(GL_DEPTH_TEST);
	
	return true;
}

bool Window::canClose()
{
	return glfwWindowShouldClose(glWindow);
}

void Window::clear()
{
	glClearColor(0.2f, 0.3f, 0.3f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
}

void Window::swap()
{
	glfwSwapBuffers(glWindow);
}

void Window::destroy()
{
	glfwTerminate();
}

// CALLBACKS
// =========
void Window::framebuffer_size_callback(GLFWwindow* window, int width, int height)
{
    glViewport(0, 0, width, height);
}
