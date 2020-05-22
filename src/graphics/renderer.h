#pragma once
#include "../data/structs.h"

Cube* CubeCreate(const char *directory, const char *filename);
void CubeDraw(Cube* cube, Vec3 *position, float width, float height, Vec3 *tintColor);

Sprite* SpriteCreate(const char *directory, const char *filename);
void SpriteDraw(Sprite *sprite, Vec3 *position, float width, float height, Vec3 *tintColor);

RenderObject* InitRenderObject(float *vertices, int verticesSize, int indicesSize, int stride, unsigned int* attributeSizes, unsigned int attributeCount);
void RendererDraw(RenderObject* rendererObject, Vec3 *position, float width, float height, Vec3 *tintColor);
