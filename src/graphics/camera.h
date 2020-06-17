#pragma once
#include "../data/structs.h"
#include "../external/cglm/cglm.h"

enum Direction {
	FORWARD,
	BACKWARD,
	LEFT,
	RIGHT,
	UP,
	DOWN
};

enum CameraProjection {
	PERSPECTIVE = 0,
	ORTHOGRAPHIC
};

typedef struct Camera {
	enum CameraProjection projection;
	unsigned int size;
	float fov;
	vec3 position;
	vec3 front;
	vec3 up;
	vec3 right;
	float yaw;
	float pitch;
} Camera;


Camera* CameraCreate();
void CameraUpdateVectors(Camera* camera);
void CameraMove(Camera *camera, enum Direction direction, float velocity);
