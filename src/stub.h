#pragma once
#include "data/structs.h"

// REBEL
//
void RebelInit(unsigned int windowWidth, unsigned int windowHeight, const char* windowName);
void RebelDestroy();

void InputProcess();

// VECTOR
//
Vec3* Vec3Create(float x, float y, float z);

// CAMERA
//
Camera* CameraGetMain();

// WINDOW
//
bool WindowCanClose();
void WindowClear();
void WindowSwap();
void WindowDestroy();

// KEYBOARD
//
bool KeyIsDown(enum Keys key);
bool KeyIsUp(enum Keys key);

// SPRITE
Sprite* SpriteCreate(const char *directory, const char *filename);
void SpriteDraw(Sprite *sprite, Vec3 *position, float width, float height, Vec3 *tintColor, Shader* shader);

// RENDERER
Cube* CubeCreate(const char *directory, const char *filename);
void CubeDraw(Cube* cube, Vec3 *position, float width, float height, Vec3 *tintColor, Shader* shader);

// SHADER
Shader* ShaderCreate(const char* vertexPath, const char* fragmentPath);
