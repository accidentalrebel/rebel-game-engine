#include "rebel.h"
#include "scripting/scripting.h"
#include <iostream>

#define STB_IMAGE_IMPLEMENTATION
#include "external/stb_image.h"

Rebel g_rebel;

void rebel::Init(unsigned int windowWidth, unsigned int windowHeight, const char* windowName)
{
	g_rebel.window = window::Init(windowWidth, windowHeight, windowName);
	g_rebel.defaultShader = shader::Create("shaders/simple.vs", "shaders/simple.fs");
	g_rebel.scriptCtx = scripting::Init();
}

void rebel::ProcessInputs()
{
	glfwPollEvents();

	if(glfwGetKey(g_rebel.window.glWindow, GLFW_KEY_ESCAPE) == GLFW_PRESS)
		glfwSetWindowShouldClose(g_rebel.window.glWindow, true);
}

void rebel::Destroy()
{
	scripting::Destroy();
	window::Destroy();
}
