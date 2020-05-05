#include "window.h"
#include <iostream>
#include "../rebel.h"

Window InitWindow(int windowWidth, int windowHeight, const char* windowName)
{
	Window window = { 0 };
	window.width = windowWidth;
	window.height = windowHeight;
	
	glfwInit();
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
	//glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE); // Uncomment for MacOSX

	window.glWindow = glfwCreateWindow(windowWidth, windowHeight, windowName, NULL, NULL);
	if (window.glWindow == NULL)
	{
		std::cout << "Failed to create glfwWindow" << std::endl;
    glfwTerminate();
		// return { 0 };
	}
	glfwMakeContextCurrent(window.glWindow);
	glfwSetFramebufferSizeCallback(window.glWindow, framebuffer_size_callback);
	
	if ( !gladLoadGLLoader((GLADloadproc)glfwGetProcAddress))
	{
		std::cout << "Failed to initialize GLAD" << std::endl;
		// return NULL;
	}

	glEnable(GL_DEPTH_TEST);
	
	return window;
}

bool CanCloseWindow()
{
	return glfwWindowShouldClose(g_rebel.window.glWindow);
}

void ClearWindow()
{
	glClearColor(0.2f, 0.3f, 0.3f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
}

void SwapWindows()
{
	glfwSwapBuffers(g_rebel.window.glWindow);
}

void DestroyWindow()
{
	glfwTerminate();
}

// CALLBACKS
// =========
void framebuffer_size_callback(GLFWwindow* window, int windowWidth, int windowHeight)
{
	g_rebel.window.width = windowWidth;
	g_rebel.window.height = windowHeight;

	glViewport(0, 0, windowWidth, windowHeight);
}
