#pragma once
#include "../data/structs.h"

Cube* CubeCreate(const char *directory, const char *filename);
RenderObject* InitRenderObject(float vertices[], int verticesSize, int stride, int positionSize, int texCoordSize);
