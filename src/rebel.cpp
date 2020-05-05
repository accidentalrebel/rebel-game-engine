#include "rebel.h"
#include "scripting/scripting.h"
#include <iostream>

#define STB_IMAGE_IMPLEMENTATION
#include "external/stb_image.h"

Rebel g_rebel;
Tebel g_tebel;

void RebelInit(unsigned int windowWidth, unsigned int windowHeight, const char* windowName)
{
	g_rebel.window = InitWindow(windowWidth, windowHeight, windowName);
	g_rebel.defaultShader = shader::Create("shaders/simple.vs", "shaders/simple.fs");
	g_rebel.scriptCtx = InitializeScripting();
}

void ProcessInputs()
{
	glfwPollEvents();

	if(glfwGetKey(g_rebel.window.glWindow, GLFW_KEY_ESCAPE) == GLFW_PRESS)
		glfwSetWindowShouldClose(g_rebel.window.glWindow, true);
}
