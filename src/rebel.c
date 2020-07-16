#include "rebel.h"
#include "graphics/mesh.h"
#include "graphics/model.h"
#include "graphics/renderer.h"

#define STB_IMAGE_IMPLEMENTATION
#include "external/stb_image.h"

Rebel g_rebel;

Texture* test_texture;

void RebelInit(unsigned int windowWidth, unsigned int windowHeight, const char* windowName)
{
	g_rebel.window = WindowInit(windowWidth, windowHeight, windowName);
	g_rebel.mainCamera = CameraCreate();
	g_rebel.mouse = MouseInit();

	//TODO: Make separate default shader files
	g_rebel.pointLightCount = 0;
	g_rebel.defaultShader = ShaderCreate("shaders/simple.vs", "shaders/simple.fs");

	stbi_set_flip_vertically_on_load(true);

	printf("Rebel Engine Initialized\n");
} 

void RebelDraw()
{
	// Intentionally left blank. Mostly used for debugging.
}

void InputProcess()
{
	glfwPollEvents();

	if(glfwGetKey(g_rebel.window.glWindow, GLFW_KEY_ESCAPE) == GLFW_PRESS)
		glfwSetWindowShouldClose(g_rebel.window.glWindow, true);
}

double GetCurrentTime()
{
	return glfwGetTime();
}

Camera* CameraGetMain()
{
	return g_rebel.mainCamera;
}

void RebelDestroy()
{
	WindowDestroy();
}
