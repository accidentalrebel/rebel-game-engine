#pragma once

typedef struct Vec3 {
    float x;
    float y;
    float z;
} Vec3;

#include "core/window.h"
#include "chibi/eval.h"
#include "graphics/sprite.h"
#include "graphics/shader.h"
#include "scripting/scripting.h"
#include "input/keyboard.h"

typedef struct Rebel {
	Shader defaultShader;
	Window window;
	sexp scriptCtx;
} Rebel;

extern Rebel g_rebel;

namespace rebel
{
	void Init(unsigned int windowWidth, unsigned int windowHeight, const char* windowName);
	void ProcessInputs();
	void Destroy();
}
