#pragma once
#include "mesh.h"
#include "model.h"
#include "text.h"
#include "material.h"
#include "../external/cglm/cglm.h"

typedef struct Renderer
{
	bool isWireFrameMode;
} Renderer;

Renderer* RendererInit();
void RendererDraw(Model* modelObject, vec4 drawRect, vec3 position, vec3 scale, vec3 rotation, vec4 color);
void RendererDrawText(Text* text, vec3 position, vec3 scale, vec3 rotation, vec4 color);

bool RendererIsWireFrameEnabled();
void RendererSetWireFrameMode(bool status);
