#include "rebel.h"
#include "graphics/mesh.h"
#include "graphics/model.h"
#include "graphics/renderer.h"

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

	Mesh* mesh = MeshGenerateCube(1.0f, 1.0f, 1.0f);
	model = ModelLoadFromMesh(mesh);
	model->material->textureDiffuse1 = TextureLoad("assets/textures/tile.png");

	printf("Rebel Engine Initialized\n");

	model = ModelLoad("assets/models/royalty_free_box/RoyaltyFreeBox.obj");
	/* model = ModelLoad("assets/models/backpack/backpack.obj"); */
	model->material->textureDiffuse1 = TextureLoad("assets/textures/tile.png");
}

void RebelDraw()
{
	ModelDraw(model, (vec3){ 0, 2, 0}, (vec3){ 1, 0, 1 });
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
