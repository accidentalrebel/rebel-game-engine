#include "../data/structs.h"
#include <cglm/cglm.h>

typedef struct Texture {
	unsigned int id;
	char* type;
} Texture;

typedef struct Vertex {
	vec3 position;
	vec3 normal;
	vec2 texCoords;
} Vertex;

typedef struct Mesh {
	Vertex* vertices;
} Mesh;

typedef struct Model {
	Mesh** meshes;
} Model;

Mesh* MeshCreate(Vertex* vertices)
{
	Mesh* mesh = (Mesh*)malloc(sizeof(Mesh));
	mesh->vertices = vertices;
	return mesh;
}

Model* ModelCreate(Mesh *mesh)
{
	Model* model = (Model*)malloc(sizeof(Model));
	model->meshes = (Mesh**)calloc(2, sizeof(Mesh));
	model->meshes[0] = mesh;
	return model;
}
