#include "sprite.h"
#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include "../rebel.h"

LSprite CreateSprite(const char *directory, const char *filename)
{
	std::cout << "SPRITE BEING CREATED" << std::endl;
	
	LSprite sprite;
	float vertices[] = {  
		// positions   // texCoords
		-0.5f,  0.5f,  0.0f, 1.0f,
		-0.5f, -0.5f,  0.0f, 0.0f,
		0.5f, -0.5f,  1.0f, 0.0f,

		-0.5f,  0.5f,  0.0f, 1.0f,
		0.5f, -0.5f,  1.0f, 0.0f,
		0.5f,  0.5f,  1.0f, 1.0f
	};
	std::cout << "SPRITE " << sprite.VAO << std::endl;

	glGenVertexArrays(1, &sprite.VAO);
	std::cout << "SPRITE 0.5" << std::endl;
	glGenBuffers(1, &sprite.VBO);
	
	glBindVertexArray(sprite.VAO);

	std::cout << "SPRITE 1" << std::endl;
	
	glBindBuffer(GL_ARRAY_BUFFER, sprite.VBO);
	glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), &vertices, GL_STATIC_DRAW);
	
	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(float), (void*)0);
	
	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(float), (void*)(2 * sizeof(float)));
	
	glBindVertexArray(0);

 	sprite.texture = Shader::LoadTextureFromFile(directory, filename);

	std::cout << "SPRITE CREATED" << std::endl;
	
	return sprite;
}

// void DrawSprite(LSprite sprite, glm::vec3 currentPosition, float width, float height, glm::vec3 tintColor)
void DrawSprite(LSprite *sprite, float width, float height)
{
	std::cout << "DRAWING SPRITE [" << sprite->VAO << "]" << std::endl;
	g_defaultShader->use();
	g_defaultShader->setInt("texture1", 0);

	glm::vec3 currentPosition(0.0f);
	
	float windowWidth = rebel::Rebel::instance->window->width;
	float windowHeight = rebel::Rebel::instance->window->height;
	
	// glm::mat4 projection = glm::perspective(glm::radians(30.0f), windowWidth/windowHeight, 0.1f, 100.0f);
	glm::mat4 projection = glm::ortho(0.0f, windowWidth, 0.0f, windowHeight, -100.0f, 100.0f);
	
	glm::mat4 view = glm::mat4(1.0f);
	view = glm::translate(view, glm::vec3(0.0f, 0.0f, -30.0f)); 
	
	glm::mat4 model = glm::mat4(1.0f);
	model = glm::scale(model, glm::vec3(50, 50, 1.0f));
	
	model = glm::translate(model, glm::vec3(currentPosition.x / 50, currentPosition.y / 50, currentPosition.z));

	g_defaultShader->setVec3("tint", glm::vec3(1.0f, 0.0f, 1.0f));
	g_defaultShader->setMat4("projection", projection);
	g_defaultShader->setMat4("view", view);
	g_defaultShader->setMat4("model", model);
	
	glBindVertexArray(sprite->VAO);
	glBindTexture(GL_TEXTURE_2D, sprite->texture);
	glDrawArrays(GL_TRIANGLES, 0, 6);
}

void TestFunc()
{
	std::cout << "TEST FUNC" << std::endl;
}

const char* TestFunc2()
{
	return "TEST2";
}


// using namespace rebel;

// Sprite::Sprite(Shader *shader, const char *directory, const char *filename)
// {
// 	this->shader = shader;
	
// 	float vertices[] = {  
// 		// positions   // texCoords
// 		-0.5f,  0.5f,  0.0f, 1.0f,
// 		-0.5f, -0.5f,  0.0f, 0.0f,
// 		0.5f, -0.5f,  1.0f, 0.0f,

// 		-0.5f,  0.5f,  0.0f, 1.0f,
// 		0.5f, -0.5f,  1.0f, 0.0f,
// 		0.5f,  0.5f,  1.0f, 1.0f
// 	};

// 	glGenVertexArrays(1, &VAO);
// 	glGenBuffers(1, &VBO);
	
// 	glBindVertexArray(VAO);
	
// 	glBindBuffer(GL_ARRAY_BUFFER, VBO);
// 	glBufferData(GL_ARRAY_BUFFER, sizeof(vertices), &vertices, GL_STATIC_DRAW);
	
// 	glEnableVertexAttribArray(0);
// 	glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(float), (void*)0);
	
// 	glEnableVertexAttribArray(1);
// 	glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * sizeof(float), (void*)(2 * sizeof(float)));
	
// 	glBindVertexArray(0);

//  	texture = Shader::LoadTextureFromFile(directory, filename);
// }

// void Sprite::draw(glm::vec3 currentPosition, float width, float height, glm::vec3 tintColor)
// {
// 	float windowWidth = Rebel::instance->window->width;
// 	float windowHeight = Rebel::instance->window->height;
	
//  	shader->use();
// 	shader->setInt("texture1", 0);
	
// 	// glm::mat4 projection = glm::perspective(glm::radians(30.0f), windowWidth/windowHeight, 0.1f, 100.0f);
// 	glm::mat4 projection = glm::ortho(0.0f, windowWidth, 0.0f, windowHeight, -100.0f, 100.0f);
	
// 	glm::mat4 view = glm::mat4(1.0f);
// 	view = glm::translate(view, glm::vec3(0.0f, 0.0f, -30.0f)); 
	
// 	glm::mat4 model = glm::mat4(1.0f);
// 	model = glm::scale(model, glm::vec3(width, height, 1.0f));
	
// 	model = glm::translate(model, glm::vec3(currentPosition.x / width, currentPosition.y / height, currentPosition.z));

// 	shader->setVec3("tint", tintColor);
// 	shader->setMat4("projection", projection);
// 	shader->setMat4("view", view);
// 	shader->setMat4("model", model);
	
// 	glBindVertexArray(VAO);
// 	glBindTexture(GL_TEXTURE_2D, texture);
// 	glDrawArrays(GL_TRIANGLES, 0, 6);
// }
