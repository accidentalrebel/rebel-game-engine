#pragma once

enum Direction {
	FORWARD,
	BACKWARD,
	LEFT,
	RIGHT,
	UP,
	DOWN
};

typedef struct Vec3
{
	float x;
	float y;
	float z;
} Vec3;

typedef struct Light
{
	Vec3* ambient;
	Vec3* diffuse;
	Vec3* specular;
} Light;

typedef struct DirectionLight
{
	Vec3* direction;
	Light* light;
} DirectionLight;

typedef struct PointLight
{
	Vec3* position;
	Light* light;

	float constant;
	float linear;
	float quadratic;
} PointLight;

typedef struct Mouse
{
	unsigned int initialized;
	double xPos;
	double yPos;
} Mouse;

typedef struct Shader {
	unsigned int id;
} Shader;
