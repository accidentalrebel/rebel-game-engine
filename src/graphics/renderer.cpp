#include <glad/glad.h>
#include <GLFW/glfw3.h>

#include "renderer.h"
#include <cstdlib>
#include "../external/stb_image.h"
#include "../rebel.h"
#include "shader.h"

Cube* CubeCreate(const char *directory, const char *filename)
{
	float vertices[] = {
    -0.5f, -0.5f, -0.5f,  0.0f, 0.0f,
		0.5f, -0.5f, -0.5f,  1.0f, 0.0f,
		0.5f,  0.5f, -0.5f,  1.0f, 1.0f,
		0.5f,  0.5f, -0.5f,  1.0f, 1.0f,
    -0.5f,  0.5f, -0.5f,  0.0f, 1.0f,
    -0.5f, -0.5f, -0.5f,  0.0f, 0.0f,

    -0.5f, -0.5f,  0.5f,  0.0f, 0.0f,
		0.5f, -0.5f,  0.5f,  1.0f, 0.0f,
		0.5f,  0.5f,  0.5f,  1.0f, 1.0f,
		0.5f,  0.5f,  0.5f,  1.0f, 1.0f,
    -0.5f,  0.5f,  0.5f,  0.0f, 1.0f,
    -0.5f, -0.5f,  0.5f,  0.0f, 0.0f,

    -0.5f,  0.5f,  0.5f,  1.0f, 0.0f,
    -0.5f,  0.5f, -0.5f,  1.0f, 1.0f,
    -0.5f, -0.5f, -0.5f,  0.0f, 1.0f,
    -0.5f, -0.5f, -0.5f,  0.0f, 1.0f,
    -0.5f, -0.5f,  0.5f,  0.0f, 0.0f,
    -0.5f,  0.5f,  0.5f,  1.0f, 0.0f,

		0.5f,  0.5f,  0.5f,  1.0f, 0.0f,
		0.5f,  0.5f, -0.5f,  1.0f, 1.0f,
		0.5f, -0.5f, -0.5f,  0.0f, 1.0f,
		0.5f, -0.5f, -0.5f,  0.0f, 1.0f,
		0.5f, -0.5f,  0.5f,  0.0f, 0.0f,
		0.5f,  0.5f,  0.5f,  1.0f, 0.0f,

    -0.5f, -0.5f, -0.5f,  0.0f, 1.0f,
		0.5f, -0.5f, -0.5f,  1.0f, 1.0f,
		0.5f, -0.5f,  0.5f,  1.0f, 0.0f,
		0.5f, -0.5f,  0.5f,  1.0f, 0.0f,
    -0.5f, -0.5f,  0.5f,  0.0f, 0.0f,
    -0.5f, -0.5f, -0.5f,  0.0f, 1.0f,

    -0.5f,  0.5f, -0.5f,  0.0f, 1.0f,
		0.5f,  0.5f, -0.5f,  1.0f, 1.0f,
		0.5f,  0.5f,  0.5f,  1.0f, 0.0f,
		0.5f,  0.5f,  0.5f,  1.0f, 0.0f,
    -0.5f,  0.5f,  0.5f,  0.0f, 0.0f,
    -0.5f,  0.5f, -0.5f,  0.0f, 1.0f
	}; 

	Cube* cube = (Cube*)malloc(sizeof(Cube));
	cube->renderObject = InitRenderObject(vertices, sizeof(vertices), 36, 5, 3, 2);

	stbi_set_flip_vertically_on_load(true);
 	cube->renderObject->texture = LoadTextureFromFile(directory, filename);
	return cube;
}

RenderObject* InitRenderObject(float *vertices, int verticesSize, int indicesSize, int stride, int positionSize, int texCoordSize)
{
	RenderObject* renderObject = (RenderObject*)malloc(sizeof(RenderObject));
	renderObject->indicesSize = indicesSize;

	glGenVertexArrays(1, &renderObject->VAO);
	glGenBuffers(1, &renderObject->VBO);
	
	glBindVertexArray(renderObject->VAO);

	glBindBuffer(GL_ARRAY_BUFFER, renderObject->VBO);
	glBufferData(GL_ARRAY_BUFFER, verticesSize, vertices, GL_STATIC_DRAW);

	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 5 * sizeof(float), (void*)0);
	glEnableVertexAttribArray(0);
	
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 5 * sizeof(float), (void*)(3 * sizeof(float)));
	glEnableVertexAttribArray(1);
	
	glBindVertexArray(0);
	
	return renderObject;
}

void CubeDraw(Cube* cube, Vec3 *position, float width, float height, Vec3 *tintColor, Shader* shader)
{
	RendererDraw(cube->renderObject, position, width, height, tintColor, shader);
}

void RendererDraw(RenderObject *rendererObject, Vec3 *position, float width, float height, Vec3 *tintColor, Shader* shader)
{
	Shader* shaderToUse = shader;
	if ( shaderToUse == NULL )
		shaderToUse = g_rebel.defaultShader;
		
	ShaderUse(shaderToUse);
	ShaderSetInt(shaderToUse, "texture1", 0);

	float windowWidth = g_rebel.window.width;
	float windowHeight = g_rebel.window.height;
	
	glm::mat4 projection = glm::perspective(glm::radians(45.0f), windowWidth/windowHeight, 0.1f, 100.0f);
	// glm::mat4 projection = glm::ortho(0.0f, windowWidth, 0.0f, windowHeight, -100.0f, 100.0f);
	
	glm::mat4 view = glm::mat4(1.0f);
	view = glm::translate(view, glm::vec3(0.0f, 0.0f, -3.0f)); 
	
	glm::mat4 model = glm::mat4(1.0f);
	model = glm::scale(model, glm::vec3(width * 0.02, height * 0.02, 1.0f));
	
	model = glm::translate(model, glm::vec3(position->x / width, position->y / height, position->z));
	model = glm::rotate(model, glm::radians(-55.0f), glm::vec3(1.0f, 0.0f, 0.0f));

	ShaderSetVec3(shaderToUse, "tint", glm::vec3(tintColor->x, tintColor->y, tintColor->z));
	ShaderSetMat4(shaderToUse, "projection", projection);
	ShaderSetMat4(shaderToUse, "view", view);
	ShaderSetMat4(shaderToUse, "model", model);
	
	glBindVertexArray(rendererObject->VAO);
	glBindTexture(GL_TEXTURE_2D, rendererObject->texture);
	// glDrawArrays(GL_TRIANGLES, 0, 6);
	glDrawArrays(GL_TRIANGLES, 0, rendererObject->indicesSize);
}
