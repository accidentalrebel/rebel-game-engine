#pragma once

#include <glad/glad.h>
#include "core/window.h"
#include "data/structs.h"
#include "graphics/shader.h"
#include "graphics/camera.h"

typedef struct Rebel {
	Shader *defaultShader;
	Mouse *mouse;
	Window window;

	// TODO: Camera and directionLight should be a part of a scene.
	Camera *mainCamera;
	DirectionLight* directionLight;
} Rebel;

extern Rebel g_rebel;

void RebelInit(unsigned int windowWidth, unsigned int windowHeight, const char* windowName);
void RebelDestroy();

void InputProcess();

Camera* CameraGetMain();
double GetCurrentTime();
