#pragma once
#include "data/structs.h"

typedef struct vec3test
{
	float x;
	float y;
	float z;
} vec3test;

// REBEL
// =====
void RebelTest_(Renderer* r, float v1, float v2, float v3, float x, float y);
void RebelInit(unsigned int windowWidth, unsigned int windowHeight, const char* windowName);
void RebelDraw();
void RebelDestroy();

void InputProcess();

// VECTOR
// ======
Vec3* Vec3Create(float x, float y, float z);
Vec3* Vec3Copy(Vec3* from);

// CAMERA
// ======
Camera* CameraGetMain();
void CameraUpdateVectors(Camera* camera);
void CameraMove(Camera *camera, enum Direction direction, float velocity);

// LIGHTING
// ========
DirectionLight* DirectionLightCreate(Vec3* direction, Vec3* ambient, Vec3* diffuse, Vec3* specular);
PointLight* PointLightCreate(Vec3* position, Vec3* ambient, Vec3* diffuse, Vec3* specular, float constant, float linear, float quadratic);

// WINDOW
// ======
unsigned int WindowCanClose();
void WindowClear();
void WindowSwap();
void WindowDestroy();

// KEYBOARD
// ========
unsigned int KeyIsDown(enum Keys key);
unsigned int KeyIsUp(enum Keys key);
Mouse* MouseGetInstance();
void MouseEnable();

// RENDERER
// ========
Renderer* SpriteCreate();
Renderer* CubeCreate();

// SHADER
// ======
void ShaderUse(Shader* shader);
Shader* ShaderDefault();
Shader* ShaderCreate(const char* vertexPath, const char* fragmentPath);

// TEXTURES
// ========
unsigned int TextureLoad(const char* filePath);
void TextureUnload(unsigned int textureId);

// UTILS
// =====
double GetCurrentTime();
