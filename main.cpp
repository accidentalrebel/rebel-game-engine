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
		
	while(!window->canClose())
	{
		g_engine->processInput();
		window->clear();
	}

	g_engine->destroy();
	return 0;
}
