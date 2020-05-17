#include "keyboard.h"
#include "../rebel.h"

Keys c_lastKeyPressed;

unsigned int KeyIsDown(Keys key)
{
	return (glfwGetKey(g_rebel.window.glWindow, key) == GLFW_PRESS);
}

unsigned int KeyIsUp(Keys key)
{
	if (glfwGetKey(g_rebel.window.glWindow, key) == GLFW_PRESS)
		c_lastKeyPressed = key;
	
	if (c_lastKeyPressed == key && glfwGetKey(g_rebel.window.glWindow, key) == GLFW_RELEASE)
	{
		c_lastKeyPressed = KEY_UNKNOWN;
		return 1;
	}
	return 0;
}
