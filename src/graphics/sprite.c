// This is a convenience class for creating and handling sprites.
// It still uses the model/mesh system

#include "sprite.h"

Model* SpriteCreate(float width, float height)
{
	return ModelLoadFromMesh(MeshGeneratePlane(width, height));
}

void SpriteLoadTexture(Model* sprite, Texture* texture)
{
	ModelLoadTexture(sprite, texture);
}
