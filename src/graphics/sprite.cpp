#include "sprite.h"
#include <glad/glad.h>
#include <GLFW/glfw3.h>

using namespace rebel;

void Sprite::initialize(Shader *shader, const char *directory, const char *filename)
{
	this->shader = shader;
	
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

 	texture = Shader::LoadTextureFromFile(directory, filename);
}

void Sprite::draw(glm::vec3 currentPosition, float width, float height, glm::vec3 tintColor)
{
 	shader->use();
	shader->setInt("texture1", 0);
	
	// glm::mat4 projection = glm::perspective(glm::radians(30.0f), 800.0f/600.0f, 0.1f, 100.0f);
	glm::mat4 projection = glm::ortho(0.0f, 800.0f, 0.0f, 600.0f, -100.0f, 100.0f);
	
	glm::mat4 view = glm::mat4(1.0f);
	view = glm::translate(view, glm::vec3(0.0f, 0.0f, -30.0f)); 
	
	glm::mat4 model = glm::mat4(1.0f);
	model = glm::scale(model, glm::vec3(width, height, 1.0f));
	
	model = glm::translate(model, glm::vec3(currentPosition.x / width, currentPosition.y / height, currentPosition.z));

	shader->setVec3("tint", tintColor);
	shader->setMat4("projection", projection);
	shader->setMat4("view", view);
	shader->setMat4("model", model);
	
	glBindVertexArray(VAO);
	glBindTexture(GL_TEXTURE_2D, texture);
	glDrawArrays(GL_TRIANGLES, 0, 6);
}
