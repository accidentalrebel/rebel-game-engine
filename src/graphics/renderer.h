#pragma once
#include "../data/structs.h"

Renderer* CubeCreate(const char *filePath);
Renderer* SpriteCreate(const char *filePath);
Renderer* RendererCreate(float *vertices, int verticesSize, int indicesSize, int stride, unsigned int* attributeSizes, unsigned int attributeCount);
void RendererDraw(Renderer* rendererObject, Vec3 *position, float width, float height, Vec3 *tintColor);
