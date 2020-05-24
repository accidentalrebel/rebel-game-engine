#include "window.h"
#include "../rebel.h"

Window WindowInit(int windowWidth, int windowHeight, const char* windowName)
{
	Window window = { 0 };
	window.width = windowWidth;
	window.height = windowHeight;
	
	glfwInit();
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
#ifdef PLATFORM
#if PLATFORM==macosx
	glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE); // Uncomment for MacOSX
#endif
#endif
	window.glWindow = glfwCreateWindow(windowWidth, windowHeight, windowName, NULL, NULL);
	if (window.glWindow == NULL)
	{
		printf("Failed to create glfwWindow\n");
    glfwTerminate();
		// return { 0 };
	}
	glfwMakeContextCurrent(window.glWindow);
	glfwSetFramebufferSizeCallback(window.glWindow, framebuffer_size_callback);
	
	if ( !gladLoadGLLoader((GLADloadproc)glfwGetProcAddress))
	{
		printf("Failed to initialize GLAD\n");
		// return NULL;
	}

	glEnable(GL_DEPTH_TEST);
	
	return window;
}

unsigned int WindowCanClose()
{
	return glfwWindowShouldClose(g_rebel.window.glWindow);
}

void WindowClear()
{
	glClearColor(0.1f, 0.1f, 0.1f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
}

void WindowSwap()
{
	glfwSwapBuffers(g_rebel.window.glWindow);
}

void WindowDestroy()
{
	glfwTerminate();
}

// CALLBACKS
// =========
void framebuffer_size_callback(GLFWwindow* window, int windowWidth, int windowHeight)
{
	g_rebel.window.width = windowWidth;
	g_rebel.window.height = windowHeight;

	glViewport(0, 0, windowWidth, windowHeight);
}
