#include <stdio.h>
#include "src/rebel.h"
#include "src/core/window.h"

using namespace rebel;

int main()
{
	Rebel::initialize();
	Window window(800, 600);
	printf("Hello world!");

	while(!window.canCloseWindow())
	{
		
	}
	
	return 0;
}
