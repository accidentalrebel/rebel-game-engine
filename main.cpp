#include "src/rebel.h"
#include <iostream>
#include "chibi/eval.h"

using namespace rebel;

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
	sexp_init_library(ctx, NULL, 3, sexp_context_env(ctx), sexp_version, SEXP_ABI_IDENTIFIER);
	// sexp_eval_string(ctx,"(import (scheme base) (chibi))",-1,NULL);
	sexp_eval_string(ctx,"(import (chibi))",-1,NULL);
	sexp_eval_string(ctx,"(load \"main.scm\")",-1,NULL);

	RebelInit(800, 600, "Rebel Engine");
	Sprite spr = CreateSprite("assets/textures", "tile.png");

	sexp_gc_var1(result);
	result = sexp_eval_string(ctx,"(init)",-1,NULL);
	if (sexp_exceptionp(result))
	{
    puts("FAILURE: EXCEPTION:");
		sexp_print_exception(ctx, result, SEXP_FALSE);
	}

	glm::vec3 pinkSquarePosition(405.0, 305.0f, 1.0f);

	while(!CanCloseWindow())
	{
		ProcessInputs();
		g_rebel.window->clear();

		result = sexp_eval_string(ctx,"(draw)",-1,NULL);
		if (sexp_exceptionp(result))
		{
			puts("FAILURE: EXCEPTION:");
			sexp_print_exception(ctx, result, SEXP_FALSE);
		}

		Vec3 pos;
		pos.x = pinkSquarePosition.x;
		pos.y = pinkSquarePosition.y;
		pos.z = pinkSquarePosition.z;

		Vec3 tint;
		tint.x = 1.0f;
		DrawSprite(&spr, pos, 50, 50, tint);

		if ( IsKeyDown(KEY_COMMA) )
			pinkSquarePosition.y += 1;
		if ( IsKeyDown(KEY_O) )
			pinkSquarePosition.y -= 1;
		if ( IsKeyDown(KEY_A) )
			pinkSquarePosition.x -= 1;
		if ( IsKeyDown(KEY_E) )
			pinkSquarePosition.x += 1;

		if ( IsKeyUp(KEY_PERIOD) )
 			pinkSquarePosition.z = 1;
		if ( IsKeyUp(KEY_SEMICOLON ) )
			pinkSquarePosition.z = -1;

		g_rebel.window->swap();
	}

	sexp_gc_release1(ctx);
	sexp_destroy_context(ctx);
		
	RebelDestroy();
	return 0;
}
