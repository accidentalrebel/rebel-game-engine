#pragma once

// This is a recreation of the shader closs from LearnOpenGL.com
// https://learnopengl.com/code_viewer_gh.php?code=includes/learnopengl/shader_s.h

#include <glad/glad.h>
#include <glm/glm.hpp>
#include <glm/gtc/type_ptr.hpp>

#include "../external/stb_image.h"

#include <string>
#include <fstream>
#include <sstream>
#include <iostream>

using namespace std;

typedef struct TShader {
	unsigned int id;
} TShader;

TShader CreateShader(const char* vertexPath, const char* fragmentPath);
void ShaderUse(TShader *shader);
void ShaderSetBool(TShader *shader, const string &name, bool value);
void ShaderSetInt(TShader *shader, const string &name, int value);
void ShaderSetFloat(TShader *shader, const string &name, float value);
void ShaderSetVec4(TShader *shader, const string &name, float v1, float v2, float v3, float v4);
void ShaderSetVec3(TShader *shader, const string &name, float v1, float v2, float v3);
void ShaderSetVec3(TShader *shader, const string &name, glm::vec3 v);
void ShaderSetMat4(TShader *shader, const string &name, glm::mat4 mat);
unsigned int LoadTextureFromFile(const string &directory, char const * fname);
