#include "rebel.h"
#include <iostream>

#define STB_IMAGE_IMPLEMENTATION
#include "external/stb_image.h"

using namespace rebel;

Rebel g_rebel = {};

void RebelInit(unsigned int windowWidth, unsigned int windowHeight, const char* windowName)
{
	g_rebel.window = new Window();
	if ( !g_rebel.window->initialize(windowWidth, windowHeight, windowName) )
		std::cout << "REBEL::WINDOW::Failed to create GLFW window" << std::endl;

	g_rebel.defaultShader = new Shader("shaders/simple.vs", "shaders/simple.fs");
}

void ProcessInputs()
{
	glfwPollEvents();

	if(glfwGetKey(g_rebel.window->glWindow, GLFW_KEY_ESCAPE) == GLFW_PRESS)
		glfwSetWindowShouldClose(g_rebel.window->glWindow, true);
}

bool CanCloseWindow()
{
	return g_rebel.window->canClose();
}

void RebelDestroy()
{
	g_rebel.window->destroy();
}
