#include "../rebel.h"
#include "../data/vec3.hpp"
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
	glm::vec3 front;
	glm::vec3 right;
	glm::vec3 up;
	front.x = cos(glm::radians(camera->yaw)) * cos(glm::radians(camera->pitch));
	front.y = sin(glm::radians(camera->pitch));
	front.z = sin(glm::radians(camera->yaw)) * cos(glm::radians(camera->pitch));
	front = glm::normalize(front);

	right = glm::normalize(glm::cross(front, glm::vec3(0, 1, 0)));
	up = glm::normalize(glm::cross(right, front));
	
	Vec3FromGlm(camera->front, front);
	Vec3FromGlm(camera->right, right);
	Vec3FromGlm(camera->up, up);
}

void CameraMove(Camera *camera, enum Direction direction, float velocity)
{
	glm::vec3 front = Vec3ToGlm(*camera->front);
	glm::vec3 position = Vec3ToGlm(*camera->position);
	glm::vec3 right = Vec3ToGlm(*camera->right);

	switch ( direction )
	{
	 case FORWARD:
		 position += front * velocity;
		 break;
	 case BACKWARD:
		 position -= front * velocity;
		 break;
	 case LEFT:
		 position -= right * velocity;
		 break;
	 case RIGHT:
		 position += right * velocity;
		 break;
	 default:
		 break;
	}

	Vec3FromGlm(camera->position, position);
}
