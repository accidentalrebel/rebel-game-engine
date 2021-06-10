#pragma once
#include "mesh.h"
#include "model.h"
#include "material.h"
#include "../external/cglm/cglm.h"

typedef struct Renderer
{
	bool isWireFrameMode;
} Renderer;

Renderer* RendererInit();
void RendererDraw(Model* modelObject, vec3 position, vec3 scale, vec3 rotation, vec4 color);

bool RendererIsWireFrameEnabled();
void RendererSetWireFrameMode(bool status);
