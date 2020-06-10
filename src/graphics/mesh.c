#include "mesh.h"
#include "material.h"
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
	model->material = MaterialCreate();
	return model;
}

void MeshSetup(Mesh* mesh)
{
	glGenVertexArrays(1, &mesh->VAO);
	glGenBuffers(1, &mesh->VBO);
	
	glBindVertexArray(mesh->VAO);

	glBindBuffer(GL_ARRAY_BUFFER, mesh->VBO);

	printf(">>>> %i\n", mesh->verticesSize * sizeof(Vertex));
	printf(">>>> %i\n", sizeof(Vertex));
	printf(">>>> %i\n", sizeof(float));

	int stride = 8;
	
	float* vertices = malloc(mesh->verticesSize * sizeof(Vertex));
	for ( unsigned int i = 0 ; i < mesh->verticesSize ; i++ )
	{
		vertices[i * stride + 0] = mesh->vertices[i]->position[0];
		vertices[i * stride + 1] = mesh->vertices[i]->position[1];
		vertices[i * stride + 2] = mesh->vertices[i]->position[2];
		vertices[i * stride + 3] = mesh->vertices[i]->normal[0];
		vertices[i * stride + 4] = mesh->vertices[i]->normal[1];
		vertices[i * stride + 5] = mesh->vertices[i]->normal[2];
		vertices[i * stride + 6] = mesh->vertices[i]->texCoords[0];
		vertices[i * stride + 7] = mesh->vertices[i]->texCoords[1];
	}

	printf(">>> %f\n", vertices[(18 * 8)]);
	printf(">>> %f\n", vertices[(18 * 8) + 1]);
	printf(">>> %f\n", vertices[(18 * 8) + 2]);
	printf(">>> %f\n", vertices[(19 * 8)]);
	printf(">>> %f\n", vertices[(19 * 8) + 1]);
	printf(">>> %f\n", vertices[(19 * 8) + 2]);

	printf("\n>>> %f\n", mesh->vertices[18]->position[0]);
	printf(">>> %f\n", mesh->vertices[18]->position[1]);
	printf(">>> %f\n", mesh->vertices[18]->position[2]);
	printf(">>> %f\n", mesh->vertices[19]->position[0]);
	printf(">>> %f\n", mesh->vertices[19]->position[1]);
	printf(">>> %f\n", mesh->vertices[19]->position[2]);

	glBufferData(GL_ARRAY_BUFFER, mesh->verticesSize * sizeof(Vertex), vertices, GL_STATIC_DRAW);
	free(vertices);

	glEnableVertexAttribArray(0);
	glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, 8 * sizeof(float), (void*)0);

	glEnableVertexAttribArray(1);
	glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, 8 * sizeof(float), (void*)(3 * sizeof(float)));//offsetof(Vertex, normal));

	glEnableVertexAttribArray(2);
	glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, 8 * sizeof(float), (void*)(6 * sizeof(float))); //offsetof(Vertex, texCoords));
	
	glBindVertexArray(0);
}

void ParseVertex(Mesh* mesh, float *vertices, int verticesSize, int stride)
{
	mesh->vertices = (Vertex**)calloc(verticesSize, sizeof(Vertex));
	mesh->verticesSize = verticesSize;
	
	for ( unsigned int i = 0; i < verticesSize ; i++ )
	{
		mesh->vertices[i] = (Vertex*)malloc(sizeof(Vertex));
		mesh->vertices[i]->position[0] = vertices[i * stride + 0];
		mesh->vertices[i]->position[1] = vertices[i * stride + 1];
		mesh->vertices[i]->position[2] = vertices[i * stride + 2];
		mesh->vertices[i]->normal[0] = vertices[i * stride + 3];
		mesh->vertices[i]->normal[1] = vertices[i * stride + 4];
		mesh->vertices[i]->normal[2] = vertices[i * stride + 5];
		mesh->vertices[i]->texCoords[0] = vertices[i * stride + 6];
		mesh->vertices[i]->texCoords[1] = vertices[i * stride + 7];

		if ( i == 19 )
		{
			printf(">>> %f\n", vertices[i * stride + 0]);
			printf(">>> %f\n", vertices[i * stride + 1]);
			printf(">>> %f\n", vertices[i * stride + 2]);
			printf(">>> %f\n", mesh->vertices[i]->position[0]);
			printf(">>> %f\n", mesh->vertices[i]->position[1]);
			printf(">>> %f\n\n", mesh->vertices[i]->position[2]);
		}

		// TODO: Find out what went wrong below
		/* float x = 0; */
		/* float y = 0; */
		/* float z = 0; */
		/* Vertex* vertex = (Vertex*)malloc(sizeof(Vertex)); */
		/* for ( unsigned int j = 0; j < stride ; j++ ) */
		/* { */
		/* 	if ( j == 0 || j == 3 || j == 6 ) */
		/* 		x = vertices[(i * stride) + j]; */
		/* 	else if ( j == 1 || j == 4 || j == 7 ) */
		/* 		y = vertices[(i * stride) + j]; */
		/* 	else if ( j == 2 || j == 5 ) */
		/* 		z = vertices[(i * stride) + j]; */
			
		/* 	if ( j == 2 ) */
		/* 	{ */
		/* 		vertex->position[0] = x; */
		/* 		vertex->position[1] = y; */
		/* 		vertex->position[2] = z; */
		/* 	} */
		/* 	else if ( j == 5 ) */
		/* 	{ */
		/* 		vertex->normal[0] = x; */
		/* 		vertex->normal[1] = y; */
		/* 		vertex->normal[2] = z; */
		/* 	} */
		/* 	else if ( j == 7 ) */
		/* 	{ */
		/* 		vertex->texCoords[0] = x; */
		/* 		vertex->texCoords[1] = y; */
		/* 	} */
		/* } */
		/* mesh->vertices[i] = vertex; */
	}
}
