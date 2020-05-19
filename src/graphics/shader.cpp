#include "shader.h"
#include "../core/utils.h"

Shader* ShaderCreate(const char* vertexPath, const char* fragmentPath)
{
	const char* vShaderCode = UtilsReadFile(vertexPath);
	const char* fShaderCode = UtilsReadFile(fragmentPath);

	unsigned int vertex, fragment;
	int success;
	char infoLog[512];

	// VERTEX
	vertex = glCreateShader(GL_VERTEX_SHADER);
	glShaderSource(vertex, 1, &vShaderCode, NULL);
	glCompileShader(vertex);

	glGetShaderiv(vertex, GL_COMPILE_STATUS, &success);
	if (!success)
	{
		glGetShaderInfoLog(vertex, 512, NULL, infoLog);
		sprintf("ERROR::SHADER::VERTEX::COMPILATION_FAILED %s\n", infoLog);
	}

	// FRAGMENT
	fragment = glCreateShader(GL_FRAGMENT_SHADER);
	glShaderSource(fragment, 1, &fShaderCode, NULL);
	glCompileShader(fragment);

	glGetShaderiv(fragment, GL_COMPILE_STATUS, &success);
	if (!success)
	{
		glGetShaderInfoLog(fragment, 512, NULL, infoLog);
		sprintf("ERROR::SHADER::FRAGMENT::COMPILATION_FAILED %s\n", infoLog);
	}

	Shader *shader = (Shader*)malloc(sizeof(Shader));
	
	// Shader program
	shader->id = glCreateProgram();
	glAttachShader(shader->id, vertex);
	glAttachShader(shader->id, fragment);
	glLinkProgram(shader->id);

	glGetProgramiv(shader->id, GL_LINK_STATUS, &success);
	if (!success)
	{
		glGetProgramInfoLog(shader->id, 512, NULL, infoLog);
		sprintf("ERROR::SHADER::PROGRAM::LINKING_FAILED %s\n", infoLog);
	}

	glDeleteShader(vertex);
	glDeleteShader(fragment);

	return shader;
}

void ShaderUse(Shader *shader)
{
	glUseProgram(shader->id);
}
void ShaderSetBool(Shader *shader, const char* &name, bool value)
{
	glUniform1i(glGetUniformLocation(shader->id, name), (int)value);
}
void ShaderSetInt(Shader *shader, const string &name, int value)
{
	glUniform1i(glGetUniformLocation(shader->id, name.c_str()), value);
}
void ShaderSetFloat(Shader *shader, const string &name, float value)
{
	glUniform1f(glGetUniformLocation(shader->id, name.c_str()), value);
}
void ShaderSetVec4(Shader *shader, const string &name, float v1, float v2, float v3, float v4)
{
	glUniform4f(glGetUniformLocation(shader->id, name.c_str()), v1, v2, v3, v4);
}
void ShaderSetVec3(Shader *shader, const string &name, float v1, float v2, float v3)
{
	glUniform3f(glGetUniformLocation(shader->id, name.c_str()), v1, v2, v3);
}
void ShaderSetVec3(Shader *shader, const string &name, vec3 v)
{
	glUniform3f(glGetUniformLocation(shader->id, name.c_str()), v[0], v[1], v[2]);
}
void ShaderSetMat4(Shader *shader, const string &name, mat4 mat)
{
	glUniformMatrix4fv(glGetUniformLocation(shader->id, name.c_str()), 1, GL_FALSE, (float*)mat);
}

unsigned int LoadTextureFromFile(const string &directory, char const * fname)
{
	string filename = string(fname);
	filename = directory + '/' + filename;
		
	unsigned int textureID;
	glGenTextures(1, &textureID);

	int width, height, nrComponents;
	unsigned char *data = stbi_load(filename.c_str(), &width, &height, &nrComponents, 0);
	if (data)
	{
		GLenum format;
		if (nrComponents == 1)
			format = GL_RED;
		else if (nrComponents == 3)
			format = GL_RGB;
		else if (nrComponents == 4)
			format = GL_RGBA;

		glBindTexture(GL_TEXTURE_2D, textureID);
		glTexImage2D(GL_TEXTURE_2D, 0, format, width, height, 0, format, GL_UNSIGNED_BYTE, data);
		glGenerateMipmap(GL_TEXTURE_2D);

		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, format == GL_RGBA ? GL_CLAMP_TO_EDGE : GL_REPEAT);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, format == GL_RGBA ? GL_CLAMP_TO_EDGE : GL_REPEAT);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

		stbi_image_free(data);
	}
	else
	{
		sprintf("Texture failed to load at path: %s", filename.c_str());
		stbi_image_free(data);
	}
	return textureID;
}
