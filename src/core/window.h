#pragma once
#include "../external/glad/include/glad/glad.h"
#include "../external/cglm/cglm.h"
#include <GLFW/glfw3.h>

typedef struct Window {
	GLFWwindow* glWindow;
	unsigned int width;
	unsigned int height;
} Window;

Window WindowInit(int windowWidth, int windowHeight, const char* windowName);
unsigned int WindowCanClose();
void WindowClear(vec3 color);
void WindowSwap();
void WindowDestroy();

static void framebuffer_size_callback(GLFWwindow* window, int width, int height);
