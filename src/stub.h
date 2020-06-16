#pragma once
#include "data/structs.h"

// VECTOR
// ======
Vec3* Vec3Create(float x, float y, float z);
Vec3* Vec3Copy(Vec3* from);

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
