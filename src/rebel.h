#pragma once

#include <glad/glad.h>
#include "core/window.h"
#include "data/structs.h"
#include "graphics/shader.h"
#include "graphics/camera.h"

typedef struct Rebel {
	Shader *defaultShader;
	Shader *currentShader;
	Mouse *mouse;
	Window window;

	// TODO: Camera and directionLight should be a part of a scene.
	Camera *mainCamera;
	DirectionLight* directionLight;
	PointLight* pointLights[4];
	unsigned int pointLightCount;
} Rebel;

extern Rebel g_rebel;

void RebelTest(Renderer* r, vec3 v, float x, float y);
void RebelInit(unsigned int windowWidth, unsigned int windowHeight, const char* windowName);
void RebelDestroy();

void InputProcess();

Camera* CameraGetMain();
double GetCurrentTime();
