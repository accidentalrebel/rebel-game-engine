#include "rebel.h"
#include <stdio.h>

bool rebel::Rebel::initialize(unsigned int windowWidth, unsigned int windowHeight, const char* windowName)
{
	window = new Window();
	if ( !window->initialize(windowWidth, windowHeight, windowName) )
	{
		printf("REBEL::WINDOW::Failed to create GLFW window");
	}

	return true;
}

bool rebel::Rebel::canClose()
{
	return window->canClose();
}
