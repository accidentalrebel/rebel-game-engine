#pragma once
#include "mesh.h"
#include "material.h"
#include <assimp/cimport.h>
#include <assimp/scene.h>
#include <assimp/postprocess.h>

typedef struct Model {
	Mesh** meshes;
	unsigned int meshesSize;
	Material* material;
} Model;

typedef struct ModelProcessing {
	Model* model;
	char* directory;
} ModelProcessing;

Model* ModelLoad(const char* path);
Model* ModelLoadFromMesh(Mesh* mesh);
void ModelLoadTexture(Model* model, Texture* texture);
void ModelDraw(Model* modelObject, vec3 position, vec3 color);

void ModelProcessNode(ModelProcessing* processing, const struct aiNode* node, const struct aiScene* scene, unsigned int *currentMeshIndex);
Mesh* ModelProcessMesh(ModelProcessing* processing, const struct aiMesh* mesh, const struct aiScene* scene);

void LoadMaterialTextures(ModelProcessing* processing, const struct aiMaterial *mat, enum aiTextureType type, char* typeName);
void TextureFromFile(char* path);
//unsigned int IsTextureAlreadyLoaded(Texture** loadedTextures, const char* path);
unsigned int IsTextureAlreadyLoaded(ModelProcessing* processing, const char* path);
