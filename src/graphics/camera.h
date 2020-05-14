#include "../data/structs.h"

typedef struct Camera {
	enum CameraProjection projection;
} Camera;

Camera* CameraCreate();
void CameraSetProjection(Camera* camera, enum CameraProjection projection);
