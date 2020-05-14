#include "rebel.h"
#include <iostream>

#define STB_IMAGE_IMPLEMENTATION
#include "external/stb_image.h"

Rebel g_rebel;

void RebelInit(unsigned int windowWidth, unsigned int windowHeight, const char* windowName)
{
	g_rebel.window = WindowInit(windowWidth, windowHeight, windowName);
	g_rebel.mainCamera = CameraCreate();
	g_rebel.defaultShader = ShaderCreate("shaders/simple.vs", "shaders/simple.fs");
}

void ProcessInputs()
{
	glfwPollEvents();

	if(glfwGetKey(g_rebel.window.glWindow, GLFW_KEY_ESCAPE) == GLFW_PRESS)
		glfwSetWindowShouldClose(g_rebel.window.glWindow, true);
}

Vec3* MakeVec3(float x = 0, float y = 0, float z = 0)
{
	Vec3* v = (Vec3*)malloc(sizeof(Vec3));
	v->x = x;
	v->y = y;
	v->z = z;
	return v;
}

Camera* GetMainCamera()
{
	return g_rebel.mainCamera;
}

void RebelDestroy()
{
	WindowDestroy();
}
