#pragma once
#include "../data/structs.h"
#include "mesh.h"

Renderer* CubeCreate();
Renderer* SpriteCreate();
Renderer* RendererCreate(float *vertices, int verticesSize, int indicesSize, int stride, unsigned int* attributeSizes, unsigned int attributeCount);
Renderer* RendererCreate2(Mesh* mesh, int verticesSize, int indicesSize, int stride, unsigned int* attributeSizes, unsigned int attributeCount);
void RendererDraw(Renderer* rendererObject, Vec3 *position, float width, float height);
void RendererDraw2(Model* model, Vec3 *position, float width, float height);
