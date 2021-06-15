#include <assert.h>
#include "rebel.h"
#include "graphics/mesh.h"
#include "graphics/model.h"
#include "graphics/renderer.h"
#include "core/utils.h"
#include "external/json.h"

#define STB_IMAGE_IMPLEMENTATION
#include "external/stb_image.h"

Rebel g_rebel;

Texture* test_texture;

void RebelInit(unsigned int windowWidth, unsigned int windowHeight, const char* windowName)
{
	g_rebel.window = WindowInit(windowWidth, windowHeight, windowName);
	g_rebel.mainCamera = CameraCreate();
	g_rebel.mouse = MouseInit();
	g_rebel.renderer = RendererInit();

	g_rebel.pointLightCount = 0;
	g_rebel.defaultShader = ShaderCreate("shaders/simple.vs", "shaders/simple.fs");

	stbi_set_flip_vertically_on_load(true);

	printf("Rebel Engine Initialized\n");

	char* fileName = "assets/fonts/font.fnt";
	char* fileContents = UtilsReadFile(fileName);
	printf("################ %s", fileContents);

	struct json_value_s* root = json_parse(fileContents, strlen(fileContents));
	assert(root->type == json_type_object);
	
	struct json_object_s* rootObj = (struct json_object_s*)root->payload;
	printf("########### %d\n", rootObj->length);

	struct json_object_element_s* charsObj = rootObj->start;
	printf("########### %s\n", charsObj->name->string);
	assert(strcmp(charsObj->name->string, "chars") == 0);

	struct json_value_s* chars_array_value = charsObj->value;
	assert(chars_array_value->type == json_type_array);

	struct json_array_s* charsArray = (struct json_array_s*)chars_array_value->payload;

	struct json_array_element_s* charObj = charsArray->start;
	assert(charObj->value->type == json_type_object);

	struct json_object_s* charObjE = charObj->value->payload;

	struct json_object_element_s* charDeets = charObjE->start;
	printf("########### %s/n", charDeets->name->string);
	
	free(root);
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
