#ifndef MOUSE_H
#define MOUSE_H

#include "../data/structs.h"
#include <glad/glad.h>
#include "../core/window.h"

Mouse* MouseInit();
Mouse* MouseGetInstance();
void MouseCallback(GLFWwindow* window, double xpos, double ypos);
void MouseEnable();

#endif
