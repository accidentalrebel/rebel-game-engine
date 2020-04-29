#include "rebel.h"
#include <stdio.h>
#include <glad/glad.h>

void Rebel::initialize()
{
	// Call glad.c's built in loader
	if ( gladLoadGL() )
	{
		return;
	}

	printf("OpenGL version: %s\n", glGetString(GL_VERSION));
}
