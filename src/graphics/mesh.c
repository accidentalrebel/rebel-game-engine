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
	}
}

Mesh* MeshGenerateCube(float width, float height, float length)
{
	float vertices[] = {
    -width/2, -height/2, -length/2,  0.0f,  0.0f, -1.0f,  0.0f, 0.0f,
		width/2, -height/2, -length/2,  0.0f,  0.0f, -1.0f,   1.0f, 0.0f,
		width/2,  height/2, -length/2,  0.0f,  0.0f, -1.0f,   1.0f, 1.0f,
		width/2,  height/2, -length/2,  0.0f,  0.0f, -1.0f,   1.0f, 1.0f,
    -width/2,  height/2, -length/2,  0.0f,  0.0f, -1.0f,   0.0f, 1.0f,
    -width/2, -height/2, -length/2,  0.0f,  0.0f, -1.0f,   0.0f, 0.0f,

    -width/2, -height/2,  length/2,  0.0f,  0.0f, 1.0f,  0.0f, 0.0f,
		width/2, -height/2,  length/2,  0.0f,  0.0f, 1.0f,  1.0f, 0.0f,
		width/2,  height/2,  length/2,  0.0f,  0.0f, 1.0f,  1.0f, 1.0f,
		width/2,  height/2,  length/2,  0.0f,  0.0f, 1.0f,  1.0f, 1.0f,
    -width/2,  height/2,  length/2,  0.0f,  0.0f, 1.0f,  0.0f, 1.0f,
    -width/2, -height/2,  length/2,  0.0f,  0.0f, 1.0f,  0.0f, 0.0f,

    -width/2,  height/2,  length/2, -1.0f,  0.0f,  0.0f,  1.0f, 0.0f,
    -width/2,  height/2, -length/2, -1.0f,  0.0f,  0.0f,  1.0f, 1.0f,
    -width/2, -height/2, -length/2, -1.0f,  0.0f,  0.0f,  0.0f, 1.0f,
    -width/2, -height/2, -length/2, -1.0f,  0.0f,  0.0f,  0.0f, 1.0f,
    -width/2, -height/2,  length/2, -1.0f,  0.0f,  0.0f,  0.0f, 0.0f,
    -width/2,  height/2,  length/2, -1.0f,  0.0f,  0.0f,  1.0f, 0.0f,

		width/2,  height/2,  length/2,  1.0f,  0.0f,  0.0f,  1.0f, 0.0f,
		width/2,  height/2, -length/2,  1.0f,  0.0f,  0.0f,  1.0f, 1.0f,
		width/2, -height/2, -length/2,  1.0f,  0.0f,  0.0f,  0.0f, 1.0f,
		width/2, -height/2, -length/2,  1.0f,  0.0f,  0.0f,  0.0f, 1.0f,
		width/2, -height/2,  length/2,  1.0f,  0.0f,  0.0f,  0.0f, 0.0f,
		width/2,  height/2,  length/2,  1.0f,  0.0f,  0.0f,  1.0f, 0.0f,

    -width/2, -height/2, -length/2,  0.0f, -1.0f,  0.0f,  0.0f, 1.0f,
		width/2, -height/2, -length/2,  0.0f, -1.0f,  0.0f,  1.0f, 1.0f,
		width/2, -height/2,  length/2,  0.0f, -1.0f,  0.0f,  1.0f, 0.0f,
		width/2, -height/2,  length/2,  0.0f, -1.0f,  0.0f,  1.0f, 0.0f,
    -width/2, -height/2,  length/2,  0.0f, -1.0f,  0.0f,  0.0f, 0.0f,
    -width/2, -height/2, -length/2,  0.0f, -1.0f,  0.0f,  0.0f, 1.0f,

    -width/2,  height/2, -length/2,  0.0f,  1.0f,  0.0f,  0.0f, 1.0f,
		width/2,  height/2, -length/2,  0.0f,  1.0f,  0.0f,  1.0f, 1.0f,
		width/2,  height/2,  length/2,  0.0f,  1.0f,  0.0f,  1.0f, 0.0f,
		width/2,  height/2,  length/2,  0.0f,  1.0f,  0.0f,  1.0f, 0.0f,
    -width/2,  height/2,  length/2,  0.0f,  1.0f,  0.0f,  0.0f, 0.0f,
    -width/2,  height/2, -length/2,  0.0f,  1.0f,  0.0f, 0.0f, 1.0f
	}; 

	Mesh* mesh = MeshCreate();
 	ParseVertex(mesh, vertices, 36, 8);
	MeshSetup(mesh);

	return mesh;
}
