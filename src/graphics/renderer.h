#pragma once
#include "mesh.h"
#include "model.h"
#include "material.h"
#include "../data/vec3.h"
#include "../external/cglm/cglm.h"

typedef struct Renderer
{
	unsigned int VAO;
	unsigned int VBO;
	unsigned int indicesSize;

	Material* material;
} Renderer;

Renderer* CubeCreate();
Renderer* SpriteCreate();
Renderer* RendererCreate(float *vertices, int verticesSize, int indicesSize, int stride, unsigned int* attributeSizes, unsigned int attributeCount);
Renderer* RendererCreate2(Mesh* mesh, int verticesSize, int indicesSize, int stride, unsigned int* attributeSizes, unsigned int attributeCount);
void RendererDraw(Renderer* rendererObject, vec3 position, float width, float height);
void RendererDraw2(Model* model, vec3 position, float width, float height);
