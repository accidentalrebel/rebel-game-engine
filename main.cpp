#include "src/rebel.h"
#include <iostream>

using namespace rebel;

Rebel* g_engine;

float g_lastKeyPressed = 0;

// TODO: Transparent images
// TODO: Camera system
// TODO: Save system
// TODO: Text
// TODO: Audio

int main()
{
	g_engine = new Rebel();
	g_engine->initialize(800, 600, "Rebel Engine");

	Window* window = g_engine->window;

	// TODO: Separate shader class to cpp and header file
	Shader *shader = new Shader("shaders/simple.vs", "shaders/simple.fs");

	Sprite *sprite = new rebel::Sprite();

	// NOTE: We may have no need for an initializer.
	sprite->initialize(shader, "assets/textures", "tile.png");

	glm::vec3 pinkSquarePosition(400.0, 300.0f, 1.0f);

	while(!window->canClose())
	{
		g_engine->processInput();
		window->clear();

		// TODO: Window width and height should be saved in Window class.
		// NOTE: Consider having a renderer class where you call draw.
		// Or maybe change window to renderer? 
		
		sprite->draw(glm::vec3(0.0f, 0.0f, 0.0f), 100, 100);
		sprite->draw(glm::vec3(100.0f, 0.0f, 0.0f), 100, 100);
		sprite->draw(glm::vec3(800.0f, 0.0f, 0.0f), 100, 100);
		sprite->draw(glm::vec3(800.0f, 600.0f, 0.0f), 100, 100);
		sprite->draw(glm::vec3(0.0f, 600.0f, 0.0f), 50, 50);
		sprite->draw(glm::vec3(400.0f, 300.0f, 0.0f), 100, 100);
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

		// TODO: Need an input manager to clean this up
		if ( glfwGetKey(window->glWindow, GLFW_KEY_PERIOD) == GLFW_PRESS)
		{
			g_lastKeyPressed = GLFW_KEY_PERIOD;
		}
		else if ( g_lastKeyPressed == GLFW_KEY_PERIOD && glfwGetKey(window->glWindow, GLFW_KEY_PERIOD) == GLFW_RELEASE )
		{
			pinkSquarePosition.z = 1;
			g_lastKeyPressed = 0;
		}
		
		if ( g_lastKeyPressed == 0 && glfwGetKey(window->glWindow, GLFW_KEY_SEMICOLON) == GLFW_PRESS)
		{
			g_lastKeyPressed = GLFW_KEY_SEMICOLON;
		}
		else if ( g_lastKeyPressed == GLFW_KEY_SEMICOLON && glfwGetKey(window->glWindow, GLFW_KEY_SEMICOLON) == GLFW_RELEASE )
		{
			pinkSquarePosition.z = -1;
			g_lastKeyPressed = 0;
		}

		window->swap();
	}

	g_engine->destroy();
	return 0;
}
