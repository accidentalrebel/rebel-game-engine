#pragma once
#include "../external/glad/include/glad/glad.h"
#include <GLFW/glfw3.h>

typedef struct Window {
	GLFWwindow* glWindow;
	unsigned int width;
	unsigned int height;
} Window;

Window WindowInit(int windowWidth, int windowHeight, const char* windowName);
unsigned int WindowCanClose();
void WindowClear();
void WindowSwap();
void WindowDestroy();

static void framebuffer_size_callback(GLFWwindow* window, int width, int height);
