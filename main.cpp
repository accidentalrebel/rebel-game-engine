#include "src/rebel.h"
#include <iostream>

using namespace rebel;

Rebel* g_engine;

int main()
{
	g_engine = new Rebel();
	g_engine->initialize(800, 600, "Rebel Engine");

	Window* window = g_engine->window;

	Shader *shader = new Shader("shaders/simple.vs", "shaders/simple.fs");

	Sprite *sprite = new rebel::Sprite();

	// NOTE: We may have no need for an initializer.
	sprite->initialize(shader, "assets/textures", "tile.png");

	glm::vec2 pinkSquarePosition(400.0, 300.0f);

	while(!window->canClose())
	{
		g_engine->processInput();
		window->clear();

		// TODO: Window width and height should be saved in Window class.
		// NOTE: Consider having a renderer class where you call draw.
		// Or maybe change window to renderer? 
		
		sprite->draw(glm::vec2(0.0f, 0.0f), 100, 100);
		sprite->draw(glm::vec2(100.0f, 0.0f), 100, 100);
		sprite->draw(glm::vec2(800.0f, 0.0f), 100, 100);
		sprite->draw(glm::vec2(800.0f, 600.0f), 100, 100);
		sprite->draw(glm::vec2(0.0f, 600.0f), 50, 50);
		sprite->draw(glm::vec2(400.0f, 300.0f), 100, 100);
		sprite->draw(pinkSquarePosition, 50, 50, glm::vec3(1.0f, 0.0f, 1.0f));

		// TODO: Make a simpler to use Input manager. Like Unity's "Input.GetKey"
		if ( glfwGetKey(window->glWindow, GLFW_KEY_COMMA) == GLFW_PRESS)
			pinkSquarePosition.y += 1;
		if ( glfwGetKey(window->glWindow, GLFW_KEY_O) == GLFW_PRESS)
			pinkSquarePosition.y -= 1;
		if ( glfwGetKey(window->glWindow, GLFW_KEY_A) == GLFW_PRESS)
			pinkSquarePosition.x -= 1;
		if ( glfwGetKey(window->glWindow, GLFW_KEY_E) == GLFW_PRESS)
			pinkSquarePosition.x += 1;

		window->swap();
	}

	g_engine->destroy();
	return 0;
}
