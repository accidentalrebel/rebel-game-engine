#include "mesh.h"
#include <glad/glad.h>
#include <GLFW/glfw3.h>

Mesh* MeshCreate()
{
	Mesh* mesh = (Mesh*)malloc(sizeof(Mesh));
	return mesh;
}

Model* ModelCreate(Mesh *mesh)
{
	Model* model = (Model*)malloc(sizeof(Model));
	model->meshes = (Mesh**)calloc(1, sizeof(Mesh));
	model->meshes[0] = mesh;
	return model;
}

void MeshSetup(Mesh* mesh, float* vertices)
{
	glGenVertexArrays(1, &mesh->VAO);
	glGenBuffers(1, &mesh->VBO);
	
	glBindVertexArray(mesh->VAO);

	glBindBuffer(GL_ARRAY_BUFFER, mesh->VBO);

	// TODO; The 36 should not be a magic number
	glBufferData(GL_ARRAY_BUFFER, 36 * sizeof(Vertex), &vertices[0], GL_STATIC_DRAW);

	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void*)0);

	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void*)offsetof(Vertex, normal));

	glEnableVertexAttribArray(2);
	glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void*)offsetof(Vertex, texCoords));
	
	glBindVertexArray(0);
}

void ParseVertex(Mesh* mesh, float *vertices, int verticesSize, int stride)
{
	// TTEST
	verticesSize = 36;
	printf("VerticesSize: %i\n", verticesSize);
	
	mesh->vertices = (Vertex**)calloc(verticesSize, sizeof(Vertex));
	/* mesh->verticesSize = verticesSize; */
	
	for ( unsigned int i = 0; i < verticesSize ; i++ )
	{
		float x = 0;
		float y = 0;
		float z = 0;
		Vertex* vertex = (Vertex*)malloc(sizeof(Vertex));
		for ( unsigned int j = 0; j < stride ; j++ )
		{
			if ( j == 0 || j == 3 || j == 6 )
				x = vertices[(i * stride) + j];
			else if ( j == 1 || j == 4 || j == 7 )
				y = vertices[(i * stride) + j];
			else if ( j == 2 || j == 5 )
				z = vertices[(i * stride) + j];
			
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
		}
		mesh->vertices[i] = vertex;
	}

	printf("POSX %f\n", mesh->vertices[0]->position[0]);
	printf("POSY %f\n", mesh->vertices[0]->position[1]);
	printf("POSZ %f\n", mesh->vertices[0]->position[2]);

	printf("NORMALX %f\n", mesh->vertices[0]->normal[0]);
	printf("NORMALY %f\n", mesh->vertices[0]->normal[1]);
	printf("NORMALZ %f\n", mesh->vertices[0]->normal[2]);

	printf("TEXX %f\n", mesh->vertices[0]->texCoords[0]);
	printf("TEXY %f\n", mesh->vertices[0]->texCoords[1]);

	printf("POSX %f\n", mesh->vertices[35]->position[0]);
	printf("POSY %f\n", mesh->vertices[35]->position[1]);
	printf("POSZ %f\n", mesh->vertices[35]->position[2]);

	printf("NORMALX %f\n", mesh->vertices[35]->normal[0]);
	printf("NORMALY %f\n", mesh->vertices[35]->normal[1]);
	printf("NORMALZ %f\n", mesh->vertices[35]->normal[2]);

	printf("TEXX %f\n", mesh->vertices[35]->texCoords[0]);
	printf("TEXY %f\n", mesh->vertices[35]->texCoords[1]);
}
