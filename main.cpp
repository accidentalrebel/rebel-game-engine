#include <stdio.h>
#include "src/rebel.h"

int main()
{
	Rebel rebel(800, 600);
	printf("Hello world!");

	while(!Rebel::canCloseWindow(&rebel))
	{
		
	}
	
	return 0;
}
