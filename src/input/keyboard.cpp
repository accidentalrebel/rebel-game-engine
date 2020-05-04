#include "keyboard.h"
#include "../rebel.h"

Keys c_lastKeyPressed;

bool IsKeyDown(Keys key)
{
	return (glfwGetKey(rebel::Rebel::instance->window->glWindow, key) == GLFW_PRESS);
}

bool IsKeyUp(Keys key)
{
	if (glfwGetKey(rebel::Rebel::instance->window->glWindow, key) == GLFW_PRESS)
		c_lastKeyPressed = key;
	
	if (c_lastKeyPressed == key && glfwGetKey(rebel::Rebel::instance->window->glWindow, key) == GLFW_RELEASE)
	{
		c_lastKeyPressed = KEY_UNKNOWN;
		return true;
	}
	return false;
}
