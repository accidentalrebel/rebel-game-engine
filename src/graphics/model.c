#include "model.h"
#include "mesh.h"
#include "material.h"

Model* ModelLoadFromMesh(Mesh* mesh)
{
	Model* model = (Model*)malloc(sizeof(Model));
	model->meshes = (Mesh**)calloc(1, sizeof(Mesh));
	model->meshes[0] = mesh;
	model->material = MaterialCreate();
	return model;
}
