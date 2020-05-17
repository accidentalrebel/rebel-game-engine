#pragma once

// This is a recreation of the shader closs from LearnOpenGL.com
// https://learnopengl.com/code_viewer_gh.php?code=includes/learnopengl/shader_s.h

#include <glad/glad.h>
#include <cglm/cglm.h>
#include "../data/structs.h"
#include "../external/stb_image.h"

#include <iostream>
#include <string>

using namespace std;

Shader* ShaderCreate(const char* vertexPath, const char* fragmentPath);
void ShaderUse(Shader *shader);
void ShaderSetBool(Shader *shader, const string &name, bool value);
void ShaderSetInt(Shader *shader, const string &name, int value);
void ShaderSetFloat(Shader *shader, const string &name, float value);
void ShaderSetVec4(Shader *shader, const string &name, float v1, float v2, float v3, float v4);
void ShaderSetVec3(Shader *shader, const string &name, float v1, float v2, float v3);
void ShaderSetVec3(Shader *shader, const string &name, vec3 v);
void ShaderSetMat4(Shader *shader, const string &name, mat4 mat);

unsigned int LoadTextureFromFile(const string &directory, char const * fname);
