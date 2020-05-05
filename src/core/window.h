#pragma once
#include <glad/glad.h>
#include <GLFW/glfw3.h>

typedef struct Window {
	GLFWwindow* glWindow;
	unsigned int width;
	unsigned int height;
} Window;

Window InitWindow(int windowWidth, int windowHeight, const char* windowName);

bool CanCloseWindow();
void ClearWindow();
void SwapWindows();
void DestroyWindow();

static void framebuffer_size_callback(GLFWwindow* window, int width, int height);
