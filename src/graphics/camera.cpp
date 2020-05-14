#include "camera.h"
#include <cstdlib>

Camera* CameraCreate()
{
	Camera *camera = (Camera*)malloc(sizeof(Camera));
	CameraSetProjection(camera, CameraProjection::PERSPECTIVE);
	return camera;
}

void CameraSetProjection(Camera* camera, enum CameraProjection projection)
{
	camera->projection = projection;
}
