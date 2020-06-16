#include "../rebel.h"

#include "../data/vec3.h"
#include "camera.h"

Camera* CameraCreate()
{
	Camera *camera = (Camera*)malloc(sizeof(Camera));
	camera->projection = PERSPECTIVE;
	camera->size = 5;
	camera->fov = 45;
	glm_vec3_copy((vec3){0, 0, 10}, camera->position);
	glm_vec3_copy((vec3){0, 0, 1}, camera->front);
	glm_vec3_copy((vec3){0, 1, 0}, camera->up);
	glm_vec3_copy((vec3){1, 0, 0}, camera->right);
	camera->yaw = -90.0f;
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

	glm_vec3_copy(front, camera->front);
	glm_vec3_copy(right, camera->right);
}

void CameraMove(Camera *camera, enum Direction direction, float velocity)
{
	switch ( direction )
	{
	 case FORWARD:
		 glm_vec3_scale(camera->front, velocity, camera->front);
		 glm_vec3_add(camera->position, camera->front, camera->position);
		 break;
	 case BACKWARD:
		 glm_vec3_scale(camera->front, velocity, camera->front);
		 glm_vec3_sub(camera->position, camera->front, camera->position);
		 break;
	 case LEFT:
		 glm_vec3_scale(camera->right, velocity, camera->right);
		 glm_vec3_sub(camera->position, camera->right, camera->position);
		 break;
	 case RIGHT:
		 glm_vec3_scale(camera->right, velocity, camera->right);
		 glm_vec3_add(camera->position, camera->right, camera->position);
		 break;
	 default:
		 break;
	}
}
