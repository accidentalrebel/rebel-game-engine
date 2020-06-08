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
	model->meshes = (Mesh**)calloc(2, sizeof(Mesh));
	model->meshes[0] = mesh;
	return model;
}

void MeshSetup(Mesh* mesh)
{
	glGenVertexArrays(1, &mesh->VAO);
	glGenBuffers(1, &mesh->VBO);
	
	glBindVertexArray(mesh->VAO);

	glBindBuffer(GL_ARRAY_BUFFER, mesh->VBO);
	glBufferData(GL_ARRAY_BUFFER, mesh->verticesSize, &mesh->vertices[0], GL_STATIC_DRAW);

	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void*)0);

	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void*)offsetof(Vertex, normal));

	glEnableVertexAttribArray(1);
	glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex), (void*)offsetof(Vertex, texCoords));
	
	glBindVertexArray(0);
}

void ParseVertex(Mesh* mesh, float *vertices, int verticesSize, int stride)
{
	mesh->vertices = (Vertex**)calloc(verticesSize, sizeof(Vertex));
	mesh->verticesSize = verticesSize;
	
	for ( unsigned int i = 0; i < (verticesSize / stride) ; i++ )
	{
		int x = 0;
		int y = 0;
		int z = 0;
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
			mesh->vertices[i] = vertex;
		}
	}

	MeshSetup(mesh);
}
