#include <stdio.h>
#include "src/rebel.h"

using namespace rebel;

Rebel* g_engine;

int main()
{
	g_engine = new Rebel();
	if ( !g_engine->initialize(800, 600, "Rebel Engine") )
	{
		printf("ERROR::MAIN::Error in initialization!");
	}
		
	printf("Hello world!");

	while(!g_engine->canClose())
	{
	}
	
	return 0;
}
