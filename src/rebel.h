#pragma once

#include <glad/glad.h>
#include "data/structs.h"
#include "core/window.h"
#include "graphics/sprite.h"
#include "graphics/shader.h"
#include "input/keyboard.h"

typedef struct Rebel {
	Shader *defaultShader;
	Window window;
} Rebel;

extern Rebel g_rebel;

void RebelInit(unsigned int windowWidth, unsigned int windowHeight, const char* windowName);
void RebelDestroy();

void ProcessInputs();
Vec3* MakeVec3(float x, float y, float z);
