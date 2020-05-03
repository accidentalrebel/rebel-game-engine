#pragma once

#include "graphics/shader.h"

typedef struct LSprite
{
	unsigned int VAO;
	unsigned int VBO;
	unsigned int texture;
} LSprite;

LSprite CreateSprite(const char *directory, const char *filename);
void DrawSprite(LSprite *sprite, float width, float height);

void TestFunc();
const char* TestFunc2();
	
