#pragma once
#include "../data/structs.h"

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
	Vec3* up;
	Vec3* right;
	float yaw;
	float pitch;
} Camera;


Camera* CameraCreate();
void CameraUpdateVectors(Camera* camera);
void CameraMove(Camera *camera, enum Direction direction, float velocity);
