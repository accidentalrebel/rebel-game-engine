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
	rebel::Init(800, 600, "Rebel Engine");
	Sprite spr = sprite::Create("assets/textures", "tile.png");

	scripting::Eval("(init)");
	
	glm::vec3 pinkSquarePosition(405.0, 305.0f, 1.0f);

	while(!window::CanClose())
	{
		rebel::ProcessInputs();
		window::Clear();

		scripting::Eval("(draw)");

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

	rebel::Destroy();
	return 0;
}
