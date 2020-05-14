#pragma once
#include "data/structs.h"

// REBEL
//
void RebelInit(unsigned int windowWidth, unsigned int windowHeight, const char* windowName);
void RebelDestroy();

void ProcessInputs();

// VECTOR
//
Vec3* MakeVec3(float x, float y, float z);

// CAMERA
//
Camera* GetMainCamera();

// WINDOW
//
bool WindowCanClose();
void WindowClear();
void WindowSwap();
void WindowDestroy();

// KEYBOARD
//
bool IsKeyDown(enum Keys key);
bool IsKeyUp(enum Keys key);

// SPRITE
Sprite* SpriteCreate(const char *directory, const char *filename);
void SpriteDraw(Sprite *sprite, Vec3 *position, float width, float height, Vec3 *tintColor, Shader* shader);

// RENDERER
Cube* CubeCreate(const char *directory, const char *filename);
void CubeDraw(Cube* cube, Vec3 *position, float width, float height, Vec3 *tintColor, Shader* shader);

// SHADER
Shader* ShaderCreate(const char* vertexPath, const char* fragmentPath);
