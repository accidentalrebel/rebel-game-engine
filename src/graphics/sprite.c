// This is a convenience class for creating and handling sprites.
// It still uses the model/mesh system

#include "sprite.h"

Model* SpriteCreate(float width, float height)
{
	return ModelLoadFromMesh(MeshGeneratePlane(width, height));
}

void SpriteAddTexture(Model* sprite, Texture* texture)
{
	ModelAddTexture(sprite, texture);
}
