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

namespace shader
{
	TShader Create(const char* vertexPath, const char* fragmentPath);
	void Use(TShader *shader);
	void SetBool(TShader *shader, const string &name, bool value);
	void SetInt(TShader *shader, const string &name, int value);
	void SetFloat(TShader *shader, const string &name, float value);
	void SetVec4(TShader *shader, const string &name, float v1, float v2, float v3, float v4);
	void SetVec3(TShader *shader, const string &name, float v1, float v2, float v3);
	void SetVec3(TShader *shader, const string &name, glm::vec3 v);
	void SetMat4(TShader *shader, const string &name, glm::mat4 mat);
}

unsigned int LoadTextureFromFile(const string &directory, char const * fname);

