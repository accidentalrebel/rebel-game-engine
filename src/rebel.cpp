#include "rebel.h"
#include <iostream>

using namespace rebel;

bool Rebel::initialize(unsigned int windowWidth, unsigned int windowHeight, const char* windowName)
{
	window = new Window();
	if ( !window->initialize(windowWidth, windowHeight, windowName) )
	{
		std::cout << "REBEL::WINDOW::Failed to create GLFW window" << std::endl;
	}

	return true;
}

void Rebel::processInput()
{
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

