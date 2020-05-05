#pragma once

typedef struct Vec3 {
    float x;
    float y;
    float z;
} Vec3;

#include "core/window.h"
#include "graphics/sprite.h"
#include "graphics/shader.h"
#include "input/keyboard.h"

using namespace rebel;

typedef struct Rebel {
	Shader *defaultShader;
	Window *window;
} Rebel;

extern Rebel g_rebel;

void RebelInit(unsigned int windowWidth, unsigned int windowHeight, const char* windowName);
void RebelDestroy();

bool CanCloseWindow();
void ProcessInputs();
