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

extern Shader* g_defaultShader;
extern rebel::Window* g_window;

void RebelInit(unsigned int windowWidth, unsigned int windowHeight, const char* windowName);
void RebelDestroy();

bool CanCloseWindow();
void ProcessInputs();
