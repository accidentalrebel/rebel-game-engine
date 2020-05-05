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

typedef struct Shader {
	unsigned int id;
} Shader;

namespace shader
{
	Shader Create(const char* vertexPath, const char* fragmentPath);
	void Use(Shader *shader);
	void SetBool(Shader *shader, const string &name, bool value);
	void SetInt(Shader *shader, const string &name, int value);
	void SetFloat(Shader *shader, const string &name, float value);
	void SetVec4(Shader *shader, const string &name, float v1, float v2, float v3, float v4);
	void SetVec3(Shader *shader, const string &name, float v1, float v2, float v3);
	void SetVec3(Shader *shader, const string &name, glm::vec3 v);
	void SetMat4(Shader *shader, const string &name, glm::mat4 mat);
}

unsigned int LoadTextureFromFile(const string &directory, char const * fname);

