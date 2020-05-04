#include "src/lrebel.h"
#include "src/rebel.h"
#include <iostream>
#include "chibi/eval.h"

using namespace rebel;

Rebel* g_engine;
Shader *g_shader;

// =====================================================
// NOTE: This commit contains a lot of code that explores
// how to properly integrate chibi-scheme. Beware of
// messy code everywhere. It works though!
// Will clean these up into a future commit.
// =====================================================

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
	g_shader = new Shader("shaders/simple.vs", "shaders/simple.fs");
	// LSprite sprite = CreateSprite("assets/textures", "tile.png");

	sexp_gc_var1(result);
	
	// sexp_eval_string(ctx,"(guard (err [#t] (display \"Error\") (newline) (print-exception err) (newline)) (init))",-1,NULL);
	// sexp_eval_string(ctx,"(guard (err ((display \"Error\") (newline) (print-exception err) (newline))) (init))",-1,NULL);
	result = sexp_eval_string(ctx,"(init)",-1,NULL);
	sexp_debug(ctx, "Result: ", result);
	if (sexp_exceptionp(result))
	{
    puts("FAILURE: EXCEPTION:");
		sexp_print_exception(ctx, result, SEXP_FALSE);
	}

	result = sexp_eval_string(ctx,
                            "(import (scheme base)) "
                            "(guard (e (#t)) "
                            "  (error \"test\") "
                            "  #f)",
                            -1, NULL);

	if (sexp_booleanp(result))
    printf("Success: %s\n", result == SEXP_TRUE ? "#t" : "#f");
  else if (sexp_exceptionp(result))
    puts("Failure: exception");
  else
    puts("Big failure");

	sexp_debug(ctx, "import: ", result);

	result = sexp_eval_string(ctx, "(import (scheme base))", -1, NULL);
	sexp_debug(ctx, "import: ", result);
	
	glm::vec3 pinkSquarePosition(400.0, 300.0f, 1.0f);

	while(!window->canClose())
	{
		g_engine->processInput();
		window->clear();

		// NOTE: Consider having a renderer class where you call draw.
		// Or maybe change window to renderer? 
		
		// sprite->draw(glm::vec3(0.0f, 0.0f, 0.0f), 100, 100);
		// sprite->draw(glm::vec3(100.0f, 0.0f, 0.0f), 100, 100);
		// sprite->draw(glm::vec3(800.0f, 0.0f, 0.0f), 100, 100);
		// sprite->draw(glm::vec3(800.0f, 600.0f, 0.0f), 100, 100);
		// sprite->draw(glm::vec3(0.0f, 600.0f, 0.0f), 50, 50);
		// sprite->draw(glm::vec3(400.0f, 300.0f, 0.0f), 100, 100);
		g_shader->use();
		g_shader->setInt("texture1", 0);

		glm::vec3 currentPosition(0.0f);
	
		float windowWidth = 800;//rebel::Rebel::instance->window->width;
		float windowHeight = 600; //rebel::Rebel::instance->window->height;
	
 			// glm::mat4 projection = glm::perspective(glm::radians(30.0f), windowWidth/windowHeight, 0.1f, 100.0f);
		glm::mat4 projection = glm::ortho(0.0f, windowWidth, 0.0f, windowHeight, -100.0f, 100.0f);
	
		glm::mat4 view = glm::mat4(1.0f);
		view = glm::translate(view, glm::vec3(0.0f, 0.0f, -30.0f)); 
	
		glm::mat4 model = glm::mat4(1.0f);
		model = glm::scale(model, glm::vec3(50, 50, 1.0f));
	
		model = glm::translate(model, glm::vec3(currentPosition.x / 50, currentPosition.y / 50, currentPosition.z));

		g_shader->setVec3("tint", glm::vec3(1.0f, 0.0f, 1.0f));
		g_shader->setMat4("projection", projection);
		g_shader->setMat4("view", view);
		g_shader->setMat4("model", model);
	
		// DrawSprite(&sprite, 50, 50);
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
