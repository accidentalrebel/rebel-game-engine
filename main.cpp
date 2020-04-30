#include "src/rebel.h"
#include <iostream>

using namespace rebel;

Rebel* g_engine;

int main()
{
	g_engine = new Rebel();
	if ( !g_engine->initialize(800, 600, "Rebel Engine") )
	{
		std::cout << "ERROR::MAIN::Error in initialization!" << std::endl;
	}
	Window* window = g_engine->window;
	Sprite *sprite = new rebel::Sprite();
	Shader *shader = new Shader("shaders/simple.vs", "shaders/simple.fs");
	sprite->initialize(shader);

	while(!window->canClose())
	{
		g_engine->processInput();
		window->clear();

		sprite->draw();

		window->swap();
	}

	g_engine->destroy();
	return 0;
}
