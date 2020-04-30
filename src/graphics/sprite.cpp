#include "sprite.h"
#include <glad/glad.h>
#include <GLFW/glfw3.h>

using namespace rebel;

void Sprite::initialize()
{
	float vertices[] = {  
		// positions   // texCoords
		-0.5f,  0.5f,  0.0f, 1.0f,
		-0.5f, -0.5f,  0.0f, 0.0f,
		0.5f, -0.5f,  1.0f, 0.0f,

		-0.5f,  0.5f,  0.0f, 1.0f,
		0.5f, -0.5f,  1.0f, 0.0f,
		0.5f,  0.5f,  1.0f, 1.0f
	};

	glGenVertexArrays(1, &VAO);
	glGenBuffers(1, &VBO);
	
	glBindVertexArray(VAO);
	
	glBindBuffer(GL_ARRAY_BUFFER, VBO);
	glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), &vertices, GL_STATIC_DRAW);
	
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(float), (void*)0);
	
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(float), (void*)(2 * sizeof(float)));
	
	glBindVertexArray(0);

	shader = new Shader("shaders/simple.vs", "shaders/simple.fs");
 	// texture = Shader::LoadTextureFromFile("tile.jpg", "assets/textures");
}

void Sprite::draw()
{
 	// shader->use();
	glBindVertexArray(VAO);
	glBindTexture(GL_TEXTURE_2D, texture);
	glDrawArrays(GL_TRIANGLES, 0, 6);
}

