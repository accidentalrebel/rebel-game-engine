#include "rebel.h"
#include <iostream>

#define STB_IMAGE_IMPLEMENTATION
#include "external/stb_image.h"

using namespace rebel;

Shader *g_defaultShader;

Rebel* Rebel::instance = new Rebel();

Rebel* Rebel::initialize(unsigned int windowWidth, unsigned int windowHeight, const char* windowName)
{
	Rebel::instance->window = new Window();
	if ( !Rebel::instance->window->initialize(windowWidth, windowHeight, windowName) )
	{
		std::cout << "REBEL::WINDOW::Failed to create GLFW window" << std::endl;
	}

	g_defaultShader = new Shader("shaders/simple.vs", "shaders/simple.fs");

	return instance;
}

void Rebel::processInput()
{
	glfwPollEvents();

	if(glfwGetKey(window->glWindow, GLFW_KEY_ESCAPE) == GLFW_PRESS)
		glfwSetWindowShouldClose(window->glWindow, true);
}

bool Rebel::canClose()
{
	return window->canClose();
}

void Rebel::destroy()
{
	window->destroy();
}
