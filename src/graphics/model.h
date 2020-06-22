#pragma once
#include "mesh.h"
#include "material.h"
#include <assimp/cimport.h>
#include <assimp/scene.h>
#include <assimp/postprocess.h>

typedef struct Model {
	Mesh** meshes;
	Material* material;
} Model;

void ModelLoad(const char* path);
Model* ModelLoadFromMesh(Mesh* mesh);
void ModelDraw(Model* modelObject, vec3 position, vec3 color);

void ModelProcessNode(const struct aiNode* node, const struct aiScene* scene);
Mesh* ModelProcessMesh(const struct aiMesh* mesh, const struct aiScene* scene);
