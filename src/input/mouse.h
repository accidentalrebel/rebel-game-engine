#pragma once

#include "../data/structs.h"
#include "../external/glad/include/glad/glad.h"
#include "../core/window.h"

typedef struct Mouse
{
	unsigned int initialized;
	double xPos;
	double yPos;
} Mouse;

Mouse* MouseInit();
Mouse* MouseGetInstance();
void MouseCallback(GLFWwindow* window, double xpos, double ypos);
void MouseEnable();
