#pragma once

#include "external/glad/include/glad/glad.h"
#include "core/window.h"
#include "input/mouse.h"
#include "graphics/shader.h"
#include "graphics/camera.h"
#include "graphics/renderer.h"
#include "graphics/lighting/light.h"

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
	Renderer *renderer;
} Rebel;

extern Rebel g_rebel;

void RebelInit(unsigned int windowWidth, unsigned int windowHeight, const char* windowName);
void RebelDestroy();
void RebelDraw();

void InputProcess();

Camera* CameraGetMain();
double GetCurrentTime();
