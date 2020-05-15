#include "../rebel.h"
#include "camera.h"
#include <cstdlib>

Camera* CameraCreate()
{
	Camera *camera = (Camera*)malloc(sizeof(Camera));
	camera->projection = CameraProjection::PERSPECTIVE;
	camera->size = 5;
	camera->fov = 45;
	camera->position = Vec3Create(0, 0, -10);
	camera->front = Vec3Create(0, 0, 1);
	camera->up = Vec3Create(0, 1, 0);
	return camera;
}
