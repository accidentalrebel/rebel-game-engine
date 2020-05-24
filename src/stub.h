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
Vec3* Vec3Copy(Vec3* from);

// CAMERA
//
Camera* CameraGetMain();
void CameraUpdateVectors(Camera* camera);
void CameraMove(Camera *camera, enum Direction direction, float velocity);

// LIGHTING
//
DirectionLight* DirectionLightCreate(Vec3* direction, Vec3* ambient, Vec3* diffuse, Vec3* specular);

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

// RENDERER
Renderer* SpriteCreate(const char *directory, const char *filename);
Renderer* CubeCreate(const char *directory, const char *filename);
void RendererDraw(Renderer* cube, Vec3 *position, float width, float height, Vec3 *tintColor);
void RendererSetColor(Renderer* renderObject, Vec3* color);

// SHADER
void ShaderUse(Shader* shader);
Shader* ShaderDefault();
Shader* ShaderCreate(const char* vertexPath, const char* fragmentPath);

// UTILS
double GetCurrentTime();
