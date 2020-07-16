#include "mouse.h"
#include "../rebel.h"

Mouse* MouseInit()
{
	Mouse* mouse = (Mouse*)malloc(sizeof(Mouse));
	mouse->xPos = 0.0f;
	mouse->yPos = 0.0f;
	mouse->initialized = false;
	mouse->cursorEnabled = 0;
	return mouse;
}

Mouse* MouseGetInstance()
{
	return g_rebel.mouse;
}

void MouseCallback(GLFWwindow* window, double xPos, double yPos)
{
	g_rebel.mouse->initialized = true;
	g_rebel.mouse->xPos = xPos;
	g_rebel.mouse->yPos = yPos;
}

void CursorEnable()
{
	printf("Enabling cursor.");
	glfwSetInputMode(g_rebel.window.glWindow, GLFW_CURSOR, GLFW_CURSOR_NORMAL);
	g_rebel.mouse->cursorEnabled = 1;
}

void CursorDisable()
{
	printf("Disabling cursor.");
	glfwSetInputMode(g_rebel.window.glWindow, GLFW_CURSOR, GLFW_CURSOR_DISABLED);
	g_rebel.mouse->cursorEnabled = 0;
}

void MouseEnable()
{
	CursorDisable();
	glfwSetCursorPosCallback(g_rebel.window.glWindow, MouseCallback);
}
