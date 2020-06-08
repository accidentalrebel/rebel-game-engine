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
	Vertex** vertices;
} Mesh;

typedef struct Model {
	Mesh** meshes;
} Model;

Mesh* MeshCreate(Vertex* vertices)
{
	Mesh* mesh = (Mesh*)malloc(sizeof(Mesh));
	return mesh;
}

Model* ModelCreate(Mesh *mesh)
{
	Model* model = (Model*)malloc(sizeof(Model));
	model->meshes = (Mesh**)calloc(2, sizeof(Mesh));
	model->meshes[0] = mesh;
	return model;
}

void ParseVertex(Mesh* mesh, float *vertices, int verticesSize, unsigned int* attributeSizes, unsigned int attributeCount)
{
	mesh->vertices = (Vertex**)calloc(verticesSize, sizeof(Vertex));
	for ( unsigned int i = 0; i < verticesSize ; i++ )
	{
		int x = 0;
		int y = 0;
		int z = 0;
		Vertex* vertex = (Vertex*)malloc(sizeof(Vertex));
		for ( unsigned int j = 0; j < 8 ; j++ )
		{
			if ( j == 0 || j == 3 || j == 6 )
				x = vertices[(i * 8) + j];
			else if ( j == 1 || j == 4 || j == 7 )
				y = vertices[(i * 8) + j];
			else if ( j == 2 || j == 5 )
				z = vertices[(i * 8) + j];
			
			if ( j == 2 )
			{
				vertex->position[0] = x;
				vertex->position[1] = y;
				vertex->position[2] = z;
			}
			else if ( j == 5 )
			{
				vertex->normal[0] = x;
				vertex->normal[1] = y;
				vertex->normal[2] = z;
			}
			else if ( j == 7 )
			{
				vertex->texCoords[0] = x;
				vertex->texCoords[1] = y;
			}
			mesh->vertices[i] = vertex;
		}
	}
}
