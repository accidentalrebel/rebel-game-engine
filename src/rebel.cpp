#include "rebel.h"
#include <iostream>

#define STB_IMAGE_IMPLEMENTATION
#include "external/stb_image.h"

using namespace rebel;

Shader *g_defaultShader;
Window *g_window;

void RebelInit(unsigned int windowWidth, unsigned int windowHeight, const char* windowName)
{
	g_window = new Window();
	if ( !g_window->initialize(windowWidth, windowHeight, windowName) )
		std::cout << "REBEL::WINDOW::Failed to create GLFW window" << std::endl;

	g_defaultShader = new Shader("shaders/simple.vs", "shaders/simple.fs");
}

void ProcessInputs()
{
	glfwPollEvents();

	if(glfwGetKey(g_window->glWindow, GLFW_KEY_ESCAPE) == GLFW_PRESS)
		glfwSetWindowShouldClose(g_window->glWindow, true);
}

bool CanCloseWindow()
{
	return g_window->canClose();
}

void RebelDestroy()
{
	g_window->destroy();
}
