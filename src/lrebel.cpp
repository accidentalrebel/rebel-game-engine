#include "lrebel.h"

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
