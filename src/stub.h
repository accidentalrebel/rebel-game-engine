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
void CameraUpdateVectors(Camera* camera);
void CameraMove(Camera *camera, enum Direction direction, float velocity);

// LIGHTING
//
DirectionLight* DirectionLightCreate(Vec3* direction, Vec3* color);

// WINDOW
//
unsigned int WindowCanClose();
void WindowClear();
void WindowSwap();
void WindowDestroy();

// KEYBOARD
//
unsigned int KeyIsDown(enum Keys key);
unsigned int KeyIsUp(enum Keys key);
Mouse* MouseGetInstance();
void MouseEnable();

// SPRITE
Sprite* SpriteCreate(const char *directory, const char *filename);
void SpriteDraw(Sprite *sprite, Vec3 *position, float width, float height, Vec3 *tintColor, Shader* shader);

// RENDERER
Cube* CubeCreate(const char *directory, const char *filename);
void CubeDraw(Cube* cube, Vec3 *position, float width, float height, Vec3 *tintColor, Shader* shader);

// SHADER
Shader* ShaderCreate(const char* vertexPath, const char* fragmentPath);

// UTILS
double GetCurrentTime();
