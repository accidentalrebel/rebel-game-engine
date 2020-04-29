#include <glad/glad.h>
#include "rebel.h"
#include <stdio.h>

Rebel::Rebel(int windowWidth, int windowHeight)
{
	glfwInit();
	glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
	glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
	glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
	//glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE); // Uncomment for MacOSX

	window = glfwCreateWindow(windowWidth, windowHeight, "LearnOpenGL", NULL, NULL);
	if (window == NULL)
	{
    printf("Failed to create GLFW window");
    glfwTerminate();
		return;
	}
	glfwMakeContextCurrent(window);
}

bool Rebel::canCloseWindow(Rebel* rebel)
{
	glfwSwapBuffers(rebel->window);
	glfwPollEvents();  
	
	return glfwWindowShouldClose(rebel->window);
}
