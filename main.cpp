#include "src/rebel.h"
#include <iostream>
#include "chibi/eval.h"

using namespace rebel;

Rebel* g_engine;

float g_lastKeyPressed = 0;

// TODO: Transparent images
// TODO: Camera system
// TODO: Input Manager
// TODO: 3D Model Loader
// TODO: Save system
// TODO: Text
// TODO: Audio

int main()
{
	char load_str[250];
	sexp ctx;

	ctx = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);
  sexp_load_standard_env(ctx, NULL, SEXP_SEVEN);
  sexp_load_standard_ports(ctx, NULL, stdin, stdout, stderr, 1);
	sexp_init_library(ctx,
										NULL,
										3,
										sexp_context_env(ctx),
										sexp_version,
										SEXP_ABI_IDENTIFIER);
	// sexp_eval_string(ctx,"(import (scheme base) (chibi))",-1,NULL);
	sexp_eval_string(ctx,"(import (chibi))",-1,NULL);
	sexp_eval_string(ctx,"(load \"main.scm\")",-1,NULL);

	g_engine = Rebel::initialize(800, 600, "Rebel Engine");

	Window* window = g_engine->window;

	sexp_gc_var1(result);
	result = sexp_eval_string(ctx,"(init)",-1,NULL);
	if (sexp_exceptionp(result))
	{
    puts("FAILURE: EXCEPTION:");
		sexp_print_exception(ctx, result, SEXP_FALSE);
	}
	
	glm::vec3 pinkSquarePosition(400.0, 300.0f, 1.0f);

	while(!window->canClose())
	{
		g_engine->processInput();
		window->clear();

		sexp_eval_string(ctx,"(draw)",-1,NULL);

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

	sexp_destroy_context(ctx);
		
	g_engine->destroy();
	return 0;
}
