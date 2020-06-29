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

	/* glPolygonMode(GL_FRONT_AND_BACK, GL_LINE); */
	
	printf("Rebel Engine Initialized\n");

	stbi_set_flip_vertically_on_load(true);

	/* model = ModelLoad("assets/models/royalty_free_box/RoyaltyFreeBox.obj"); */
	model = ModelLoad("assets/models/backpack/backpack.obj");
	model->material->textureDiffuse1 = TextureLoad("assets/textures/tile.png");

	Mesh* m = model->meshes[0];
	for(unsigned int i = 0; i < 3 ; i++ )
		printf("DONE >>>>>> %f,%f,%f\n", m->vertices[i]->position[0], m->vertices[i]->position[1], m->vertices[i]->position[2]);
}

void RebelDraw()
{
	ModelDraw(model, (vec3){ 0, 0, 0}, (vec3){ 1, 1, 1 });
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
