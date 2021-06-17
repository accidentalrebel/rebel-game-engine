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

typedef struct RenderOptions
{
	vec4 drawRect;
	vec3 position;
	vec3 scale;
	vec3 rotation;
	vec3 pivot;
	vec3 viewRotation;
	vec4 color;
	
} RenderOptions;

Renderer* RendererInit();
void RendererDrawEx(Model* modelObject, RenderOptions renderOptions);
void RendererDraw(Model* modelObject, vec4 drawRect, vec3 position, vec3 scale, vec3 rotation, vec4 color);
void RendererDrawTextEx(Text* text, RenderOptions renderOptions);
void RendererDrawText(Text* text, vec3 position, vec3 scale, vec3 rotation, vec3 viewRotation, vec4 color);

bool RendererIsWireFrameEnabled();
void RendererSetWireFrameMode(bool status);
