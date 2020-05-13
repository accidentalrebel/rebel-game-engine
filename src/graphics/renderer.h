#pragma once
#include "../data/structs.h"

Cube* CubeCreate(const char *directory, const char *filename);
void CubeDraw(Cube* cube, Vec3 *position, float width, float height, Vec3 *tintColor, Shader* shader);

RenderObject* InitRenderObject(float *vertices, int verticesSize, int indicesSize, int stride, int positionSize, int texCoordSize);
void RendererDraw(RenderObject* rendererObject, Vec3 *position, float width, float height, Vec3 *tintColor, Shader* shader);
