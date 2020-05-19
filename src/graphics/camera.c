#include "../rebel.h"

#include "../data/vec3.h"
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
	camera->right = Vec3Create(1, 0, 0);
	camera->yaw = 90.0f;
	camera->pitch = 0.0f;
	return camera;
}

void CameraUpdateVectors(Camera* camera)
{
	vec3 front;
	vec3 right;
	vec3 up;

	front[0] = cos(glm_rad(camera->yaw)) * cos(glm_rad(camera->pitch));
	front[1] = sin(glm_rad(camera->pitch));
	front[2] = sin(glm_rad(camera->yaw)) * cos(glm_rad(camera->pitch));
	
	glm_vec3_normalize(front);

	vec3 temp;
	glm_vec3_zero(temp);
	temp[1] = 1;

	glm_vec3_cross(front, temp, right);
	glm_vec3_normalize(right);

	glm_vec3_cross(right, front, up);
	glm_vec3_normalize(up);

	Vec3FromGlm(camera->front, front);
	Vec3FromGlm(camera->right, right);
}

void CameraMove(Camera *camera, enum Direction direction, float velocity)
{
	vec3 front;
	vec3 position;
	vec3 right;
	Vec3ToGlm(camera->front, front);
	Vec3ToGlm(camera->position, position);
	Vec3ToGlm(camera->right, right);

	switch ( direction )
	{
	 case FORWARD:
		 glm_vec3_scale(front, velocity, front);
		 glm_vec3_add(position, front, position);
		 break;
	 case BACKWARD:
		 glm_vec3_scale(front, velocity, front);
		 glm_vec3_sub(position, front, position);
		 break;
	 case LEFT:
		 glm_vec3_scale(right, velocity, right);
		 glm_vec3_sub(position, right, position);
		 break;
	 case RIGHT:
		 glm_vec3_scale(right, velocity, right);
		 glm_vec3_add(position, right, position);
		 break;
	 default:
		 break;
	}

	Vec3FromGlm(camera->position, position);
}
