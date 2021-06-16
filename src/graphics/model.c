#include "model.h"
#include "mesh.h"
#include "material.h"
#include "shader.h"
#include "texture.h"
#include "../rebel.h"
#include "../utils/string_utils.h"

Model* ModelLoadFromMesh(Mesh* mesh)
{
	Model* model = (Model*)malloc(sizeof(Model));
	model->meshesSize = 1;
	model->meshes = (Mesh**)calloc(model->meshesSize, sizeof(Mesh));
	model->meshes[0] = mesh;
	model->material = MaterialCreate();
	return model;
}

Model* ModelLoad(const char* path)
{
	ModelProcessing processing;	

	processing.directory = (char*)malloc(sizeof(char*) * 50);
	GetDirectoryFromPath(processing.directory, path);

	const struct aiScene* scene = aiImportFile(path, aiProcess_Triangulate | aiProcess_FlipUVs);
	if ( !scene || scene->mFlags & AI_SCENE_FLAGS_INCOMPLETE || !scene->mRootNode )
	{
		printf("ERROR::ASSIMP::%s\n", aiGetErrorString());
		free(processing.directory);
		return NULL;
	}

	Model* model = (Model*)malloc(sizeof(Model));
	model->material = MaterialCreate();
	
	processing.model = model;

	model->meshesSize = scene->mNumMeshes;
	model->meshes = (Mesh**)calloc(model->meshesSize, sizeof(Mesh));

	unsigned int currentMeshIndex = 0;
	
	ModelProcessNode(&processing, scene->mRootNode, scene, &currentMeshIndex);

	free(processing.directory);

	return model;
}

void ModelProcessNode(ModelProcessing* processing, const struct aiNode* node, const struct aiScene* scene, unsigned int *currentMeshIndex)
{
	for(unsigned int i = 0; i < node->mNumMeshes; i++)
	{
		struct aiMesh *mesh = scene->mMeshes[node->mMeshes[i]];
		Mesh* m = ModelProcessMesh(processing, mesh, scene);
		MeshSetup(m);
		processing->model->meshes[*currentMeshIndex] = m;
		(*currentMeshIndex)++;
	}
	for(unsigned int i = 0; i < node->mNumChildren; i++)
	{
		ModelProcessNode(processing, node->mChildren[i], scene, currentMeshIndex);
	}
}

Mesh* ModelProcessMesh(ModelProcessing* processing, const struct aiMesh* mesh, const struct aiScene* scene)
{
	Mesh* m = MeshCreate();
	m->verticesSize = mesh->mNumVertices;
	m->indicesSize = mesh->mNumFaces * 3;

	m->vertices = (Vertex**)calloc(m->verticesSize, sizeof(Vertex));
	m->indices = (unsigned int*)calloc(m->indicesSize, sizeof(unsigned int));

	// Process Vertices
	//
	for(unsigned int i = 0; i < m->verticesSize; i++)
	{
		m->vertices[i] = (Vertex*)malloc(sizeof(Vertex));

		glm_vec3_copy((vec3) { mesh->mVertices[i].x, mesh->mVertices[i].y, mesh->mVertices[i].z }, m->vertices[i]->position);
		glm_vec3_copy((vec3) { mesh->mNormals[i].x, mesh->mNormals[i].y, mesh->mNormals[i].z }, m->vertices[i]->normal);

		if ( mesh->mTextureCoords[0])
			glm_vec2_copy((vec3){mesh->mTextureCoords[0][i].x, mesh->mTextureCoords[0][i].y}, m->vertices[i]->texCoords);
		else
			glm_vec2_copy((vec3){0, 0}, m->vertices[i]->texCoords);
	}

	// Process Indices
	//
	for(unsigned int i = 0; i < mesh->mNumFaces; i++)
	{
		struct aiFace face = mesh->mFaces[i];

		// This is a test to make sure that the numIndices is 3.
		// This shouldn't happen but we're just making sure
		if ( face.mNumIndices != 3 )
			printf("WARNING::MODELPROCESSMESH::mNumIndices is %i!\n", face.mNumIndices);
		
		for(unsigned int j = 0; j < face.mNumIndices; j++)
		{
			m->indices[(i * 3) + j] = face.mIndices[j];
		}
	}

	// Process Materials
	//
	const struct aiMaterial *material = scene->mMaterials[mesh->mMaterialIndex];
	LoadMaterialTextures(processing, material, aiTextureType_DIFFUSE, "texture_diffuse");
	LoadMaterialTextures(processing, material, aiTextureType_SPECULAR, "texture_specular");
	
	return m;
}

// NOTE: Used this https://github.com/mgerdes/exercises/blob/9380001d5b75a80813fda6e9bdb436d0cbf12c95/opengl-learning/objects/model.c as reference for how to use the C-API for assimp
void LoadMaterialTextures(ModelProcessing* processing, const struct aiMaterial *mat, enum aiTextureType type, char* typeName)
{
	struct aiString path;
	aiGetMaterialTexture(mat, type, 0, &path, 0, 0, 0, 0, 0, 0);

	if ( !IsTextureAlreadyLoaded(processing, path.data) )
	{
		Texture* texture = TextureLoad(processing->directory, path.data, typeName);

		ModelLoadTexture(processing->model, texture);
		printf("INFO::MODEL::Loaded texture: ID(%i): type(%s) path(%s)\n", texture->id, texture->type, texture->path);
	}
}

unsigned int IsTextureAlreadyLoaded(ModelProcessing* processing, const char* path)
{
	Material* material = processing->model->material;

	for(unsigned int i = 0 ; i < material->loadedTexturesCount; i++ )
	{
		if ( strcmp(material->loadedTextures[i]->path, path) == 0)
		{
			return 1;
		}
	}
	return 0;
}

void ModelLoadTexture(Model* model, Texture* texture)
{
	Material* material = model->material;
	material->loadedTextures[material->loadedTexturesCount] = texture;
	material->loadedTexturesCount++;
}

void ModelDestroy(Model* model)
{
	for( unsigned int i = 0; i < model->meshesSize ; i++ )
		MeshDestroy(model->meshes[0]);

	free(model);
}
