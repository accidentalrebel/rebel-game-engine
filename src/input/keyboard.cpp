#include "keyboard.h"
#include "../rebel.h"

bool IsKeyDown(Keys key)
{
	// int glfwKey = ConvertToGLFWKey(key);
	return (glfwGetKey(rebel::Rebel::instance->window->glWindow, key) == GLFW_PRESS);
}

int ConvertToGLFWKey(Keys key)
{
	switch( key )
	{
	 case KEY_PERIOD:
		 return GLFW_KEY_PERIOD;
	 case KEY_COMMA:
		 return GLFW_KEY_COMMA;
	}

	return -1;
}


