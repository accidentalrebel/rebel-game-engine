#include "keyboard.h"
#include "../rebel.h"

bool IsKeyDown(Keys key)
{
	return (glfwGetKey(rebel::Rebel::instance->window->glWindow, key) == GLFW_PRESS);
}
