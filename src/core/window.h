#pragma once
#include <glad/glad.h>
#include <GLFW/glfw3.h>

typedef struct Window {
	GLFWwindow* glWindow;
	unsigned int width;
	unsigned int height;
} Window;

namespace window {
	Window Init(int windowWidth, int windowHeight, const char* windowName);

	bool CanClose();
	void Clear();
	void Swap();
	void Destroy();
}

static void framebuffer_size_callback(GLFWwindow* window, int width, int height);
