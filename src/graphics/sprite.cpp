#include "sprite.h"
#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include "../rebel.h"

Sprite CreateSprite(const char *directory, const char *filename)
{
	Sprite sprite;
	float vertices[] = {  
		// positions   // texCoords
		-0.5f,  0.5f,  0.0f, 1.0f,
		-0.5f, -0.5f,  0.0f, 0.0f,
		0.5f, -0.5f,  1.0f, 0.0f,

		-0.5f,  0.5f,  0.0f, 1.0f,
		0.5f, -0.5f,  1.0f, 0.0f,
		0.5f,  0.5f,  1.0f, 1.0f
	};

	glGenVertexArrays(1, &sprite.VAO);
	glGenBuffers(1, &sprite.VBO);
	
	glBindVertexArray(sprite.VAO);

	glBindBuffer(GL_ARRAY_BUFFER, sprite.VBO);
	glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), &vertices, GL_STATIC_DRAW);
	
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(float), (void*)0);
	
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(float), (void*)(2 * sizeof(float)));
	
	glBindVertexArray(0);

 	sprite.texture = LoadTextureFromFile(directory, filename);
	
	return sprite;
}

// void DrawSprite(Sprite sprite, glm::vec3 currentPosition, float width, float height, glm::vec3 tintColor)
void DrawSprite(Sprite *sprite, Vec3 position, float width, float height, Vec3 tintColor)
{
	shader::Use(&g_rebel.defaultShader);
	shader::SetInt(&g_rebel.defaultShader, "texture1", 0);

	float windowWidth = g_rebel.window.width;
	float windowHeight = g_rebel.window.height;
	
	// glm::mat4 projection = glm::perspective(glm::radians(30.0f), windowWidth/windowHeight, 0.1f, 100.0f);
	glm::mat4 projection = glm::ortho(0.0f, windowWidth, 0.0f, windowHeight, -100.0f, 100.0f);
	
	glm::mat4 view = glm::mat4(1.0f);
	view = glm::translate(view, glm::vec3(0.0f, 0.0f, -30.0f)); 
	
	glm::mat4 model = glm::mat4(1.0f);
	model = glm::scale(model, glm::vec3(50, 50, 1.0f));
	
	model = glm::translate(model, glm::vec3(position.x / 50, position.y / 50, position.z));

	shader::SetVec3(&g_rebel.defaultShader, "tint", glm::vec3(tintColor.x, tintColor.y, tintColor.z));
	shader::SetMat4(&g_rebel.defaultShader, "projection", projection);
	shader::SetMat4(&g_rebel.defaultShader, "view", view);
	shader::SetMat4(&g_rebel.defaultShader, "model", model);
	
	glBindVertexArray(sprite->VAO);
	glBindTexture(GL_TEXTURE_2D, sprite->texture);
	glDrawArrays(GL_TRIANGLES, 0, 6);
}
