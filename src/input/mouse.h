#pragma once

#include "../external/glad/include/glad/glad.h"
#include "../core/window.h"

typedef struct Mouse
{
	unsigned int initialized;
	unsigned int cursorEnabled;
	double xPos;
	double yPos;
} Mouse;

Mouse* MouseInit();
Mouse* MouseGetInstance();
void MouseCallback(GLFWwindow* window, double xpos, double ypos);
void MouseEnable();
void CursorEnable();
void CursorDisable();
