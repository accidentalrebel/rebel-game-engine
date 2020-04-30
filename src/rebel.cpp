#include "rebel.h"
#include <stdio.h>

bool rebel::Rebel::initialize()
{
	window = new Window();
	if ( !window->initialize(800, 600) )
	{
		printf("REBEL::WINDOW::Failed to create GLFW window");
	}

	return true;
}

bool rebel::Rebel::canClose()
{
	return window->canClose();
}
