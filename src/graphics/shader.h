#pragma once

// This is a recreation of the shader closs from LearnOpenGL.com
// https://learnopengl.com/code_viewer_gh.php?code=includes/learnopengl/shader_s.h

#include <glad/glad.h>
#include <cglm/cglm.h>
#include "../data/structs.h"
#include "../external/stb_image.h"

#include <string.h>

Shader* ShaderDefault();
Shader* ShaderCreate(const char* vertexPath, const char* fragmentPath);
void ShaderUse(Shader *shader);
void ShaderSetBool(Shader *shader, const char* name, bool value);
void ShaderSetInt(Shader *shader, const char* name, int value);
void ShaderSetFloat(Shader *shader, const char* name, float value);
void ShaderSetVec4(Shader *shader, const char* name, float v1, float v2, float v3, float v4);
void ShaderSetVec3Ex(Shader *shader, const char* name, float v1, float v2, float v3);
void ShaderSetVec3(Shader *shader, const char* name, vec3 v);
void ShaderSetMat4(Shader *shader, const char* name, mat4 mat);

unsigned int LoadTextureFromFile(const char* directory, const char * fname);
