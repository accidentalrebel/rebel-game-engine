#include "shader.h"
#include "../rebel.h"
#include "../core/utils.h"

Shader* ShaderDefault()
{
	return g_rebel.defaultShader;
}

Shader* ShaderCreate(const char* vertexPath, const char* fragmentPath)
{
	char* vShaderCode = UtilsReadFile(vertexPath);
	if (vShaderCode == NULL)
	{
		printf("ERROR::SHADER::VERTEX::SHADER_LOAD_FAILED %s\n", vertexPath);
		exit(1);
	}
		
	char* fShaderCode = UtilsReadFile(fragmentPath);
	if (fShaderCode == NULL)
	{
		printf("ERROR::SHADER::VERTEX::SHADER_LOAD_FAILED %s\n", fragmentPath);
		exit(1);
	}

	unsigned int vertex, fragment;
	int success;
	char infoLog[512];

	// VERTEX
	vertex = glCreateShader(GL_VERTEX_SHADER);
	glShaderSource(vertex, 1, (const GLchar **)&vShaderCode, NULL);
	glCompileShader(vertex);

	glGetShaderiv(vertex, GL_COMPILE_STATUS, &success);
	if (!success)
	{
		glGetShaderInfoLog(vertex, 512, NULL, infoLog);
		printf("ERROR::SHADER::VERTEX::COMPILATION_FAILED %s\n", infoLog);
	}

	// FRAGMENT
	fragment = glCreateShader(GL_FRAGMENT_SHADER);
	glShaderSource(fragment, 1, (const GLchar **)&fShaderCode, NULL);
	glCompileShader(fragment);

	glGetShaderiv(fragment, GL_COMPILE_STATUS, &success);
	if (!success)
	{
		glGetShaderInfoLog(fragment, 512, NULL, infoLog);
		printf("ERROR::SHADER::FRAGMENT::COMPILATION_FAILED %s\n", infoLog);
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
		printf("ERROR::SHADER::PROGRAM::LINKING_FAILED %s\n", infoLog);
	}

	glDeleteShader(vertex);
	glDeleteShader(fragment);

	free(vShaderCode);
	free(fShaderCode);

	return shader;
}

void ShaderUse(Shader *shader)
{
	glUseProgram(shader->id);
	g_rebel.currentShader = shader;
}
void ShaderSetBool(Shader *shader, const char* name, bool value)
{
	glUniform1i(glGetUniformLocation(shader->id, name), (int)value);
}
void ShaderSetInt(Shader *shader, const char* name, int value)
{
	glUniform1i(glGetUniformLocation(shader->id, name), value);
}
void ShaderSetFloat(Shader *shader, const char* name, float value)
{
	glUniform1f(glGetUniformLocation(shader->id, name), value);
}
void ShaderSetVec4Ex(Shader *shader, const char* name, float v1, float v2, float v3, float v4)
{
	glUniform4f(glGetUniformLocation(shader->id, name), v1, v2, v3, v4);
}
void ShaderSetVec4(Shader *shader, const char* name, vec4 v)
{
	glUniform4f(glGetUniformLocation(shader->id, name), v[0], v[1], v[2], v[3]);
}
void ShaderSetVec3Ex(Shader *shader, const char* name, float v1, float v2, float v3)
{
	glUniform3f(glGetUniformLocation(shader->id, name), v1, v2, v3);
}
void ShaderSetVec3(Shader *shader, const char* name, vec3 v)
{
	glUniform3f(glGetUniformLocation(shader->id, name), v[0], v[1], v[2]);
}
void ShaderSetMat4(Shader *shader, const char* name, mat4 mat)
{
	glUniformMatrix4fv(glGetUniformLocation(shader->id, name), 1, GL_FALSE, (float*)mat);
}
