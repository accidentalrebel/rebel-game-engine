#pragma once

#include <glad/glad.h>
#include "core/window.h"
#include "data/structs.h"
#include "graphics/shader.h"
#include "graphics/camera.h"

typedef struct Rebel {
	Shader *defaultShader;
	Camera *mainCamera;
	Window window;
} Rebel;

extern Rebel g_rebel;

void RebelInit(unsigned int windowWidth, unsigned int windowHeight, const char* windowName);
void RebelDestroy();

void ProcessInputs();
Vec3* MakeVec3(float x, float y, float z);
