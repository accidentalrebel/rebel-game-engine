#include "src/rebel.h"
#include <iostream>

// TODO: Transparent images
// TODO: Camera system
// TODO: Input Manager
// TODO: 3D Model Loader
// TODO: Save system
// TODO: Text
// TODO: Audio

int main()
{
	std::cout << g_tebel.x << std::endl;
	
	rebel::Init(800, 600, "Rebel Engine");
	Sprite spr = sprite::Create("assets/textures", "tile.png");

	sexp_gc_var1(result);
	result = sexp_eval_string(g_rebel.scriptCtx,"(init)",-1,NULL);
	if (sexp_exceptionp(result))
	{
    puts("FAILURE: EXCEPTION:");
		sexp_print_exception(g_rebel.scriptCtx, result, SEXP_FALSE);
	}

	glm::vec3 pinkSquarePosition(405.0, 305.0f, 1.0f);

	while(!window::CanClose())
	{
		rebel::ProcessInputs();
		window::Clear();

		result = sexp_eval_string(g_rebel.scriptCtx,"(draw)",-1,NULL);
		if (sexp_exceptionp(result))
		{
			puts("FAILURE: EXCEPTION:");
			sexp_print_exception(g_rebel.scriptCtx, result, SEXP_FALSE);
		}

		Vec3 pos;
		pos.x = pinkSquarePosition.x;
		pos.y = pinkSquarePosition.y;
		pos.z = pinkSquarePosition.z;

		Vec3 tint = {};
		tint.x = 1.0f;
		sprite::Draw(&spr, pos, 50, 50, tint);

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

		window::Swap();
	}

	sexp_gc_release1(g_rebel.scriptCtx);
	rebel::Destroy();
	return 0;
}
