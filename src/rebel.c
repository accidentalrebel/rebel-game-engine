#include "rebel.h"
#include "input/mouse.h"
#include "graphics/mesh.h"
#include "graphics/renderer.h"
#include "data/vec3.h"

#define STB_IMAGE_IMPLEMENTATION
#include "external/stb_image.h"

Rebel g_rebel;

Renderer* testRenderer;
Model* model;

void RebelInit(unsigned int windowWidth, unsigned int windowHeight, const char* windowName)
{
	g_rebel.window = WindowInit(windowWidth, windowHeight, windowName);
	g_rebel.mainCamera = CameraCreate();
	g_rebel.mouse = MouseInit();

	//TODO: Make separate default shader files
	g_rebel.pointLightCount = 0;
	g_rebel.defaultShader = ShaderCreate("shaders/simple.vs", "shaders/simple.fs");

	Mesh* mesh = MeshGenerateCube();
	model = ModelCreate(mesh);
	model->material->textureDiffuse1 = TextureLoad("assets/textures/tile.png");
}

void RebelDraw()
{
	RendererDraw2(model, Vec3Create(0, 0, 0), 1.0f, 1.0f);
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
