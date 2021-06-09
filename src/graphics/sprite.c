#include "sprite.h"

// This is a convenience function for creating sprites.
// It still uses the model/mesh system
Model* SpriteCreate(float width, float height)
{
	return ModelLoadFromMesh(MeshGeneratePlane(width, height));
}
