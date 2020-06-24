#include "model.h"
#include "mesh.h"
#include "material.h"
#include "shader.h"
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
	Model* model = (Model*)malloc(sizeof(Model));
	model->material = MaterialCreate();

	char directory[50];
	GetDirectoryFromPath(directory, path);
	
	const struct aiScene* scene = aiImportFile(path, aiProcess_Triangulate | aiProcess_FlipUVs);
	if ( !scene || scene->mFlags & AI_SCENE_FLAGS_INCOMPLETE || !scene->mRootNode )
	{
		printf("ERROR::ASSIMP::%s\n", aiGetErrorString());
		return NULL;
	}

	model->meshesSize = scene->mNumMeshes;
	model->meshes = (Mesh**)calloc(model->meshesSize, sizeof(Mesh));

	unsigned int currentMeshIndex = 0;
	
	ModelProcessNode(model, scene->mRootNode, scene, &currentMeshIndex, directory);

	return model;
}

void ModelProcessNode(Model* model, const struct aiNode* node, const struct aiScene* scene, unsigned int *currentMeshIndex, char* directory)
{
	for(unsigned int i = 0; i < node->mNumMeshes; i++)
	{
		struct aiMesh *mesh = scene->mMeshes[node->mMeshes[i]];
		Mesh* m = ModelProcessMesh(mesh, scene, directory);
		MeshSetup(m);
		model->meshes[*currentMeshIndex] = m;
		(*currentMeshIndex)++;
	}
	for(unsigned int i = 0; i < node->mNumChildren; i++)
	{
		ModelProcessNode(model, node->mChildren[i], scene, currentMeshIndex, directory);
	}
}

Mesh* ModelProcessMesh(const struct aiMesh* mesh, const struct aiScene* scene, char* directory)
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
	LoadMaterialTextures(material, aiTextureType_DIFFUSE, "texture_diffuse", directory);
	
	return m;
}

// NOTE: Used this https://github.com/mgerdes/exercises/blob/9380001d5b75a80813fda6e9bdb436d0cbf12c95/opengl-learning/objects/model.c as reference for how to use the C-API for assimp
void LoadMaterialTextures(const struct aiMaterial *mat, enum aiTextureType type, char* typeName, char* directory)
{
	struct aiString path;
	aiGetMaterialTexture(mat, type, 0, &path, 0, 0, 0, 0, 0, 0);

	Texture* texture = (Texture*)malloc(sizeof(Texture));

	char fullPath[100];
	strcat(fullPath, directory);
	strcat(fullPath, path.data);
	
	texture->id = TextureLoad(fullPath);
	texture->type = typeName;
	texture->path = path.data;

	printf("INFO::MODEL::Loaded texture: ID(%i): type(%s) path(%s)\n", texture->id, texture->type, texture->path);
}

void ModelDraw(Model* modelObject, vec3 position, vec3 color)
{
	Shader* shaderToUse;
	if ( g_rebel.currentShader != NULL )
		shaderToUse = g_rebel.currentShader;
	else
		shaderToUse = g_rebel.defaultShader;
		
	float windowWidth = g_rebel.window.width;
	float windowHeight = g_rebel.window.height;

	mat4 projection = GLM_MAT4_IDENTITY_INIT;
	mat4 view = GLM_MAT4_IDENTITY_INIT;
	mat4 model = GLM_MAT4_IDENTITY_INIT;
	
	float screenRatio = windowWidth / windowHeight;
	float zoom = glm_rad(g_rebel.mainCamera->fov);
	float size = g_rebel.mainCamera->size;
	
	if ( g_rebel.mainCamera->projection == PERSPECTIVE )  {
		glm_perspective(zoom, screenRatio, 0.1f, 100.0f, projection);
	}
	else {
		glm_ortho(-windowWidth / 30 / size, windowWidth / 30 / size, -windowHeight / 30 / size, windowHeight / 30 / size, -100.0f, 100.0f, projection);
	}

	vec3 temp;
	glm_vec3_add(g_rebel.mainCamera->position, g_rebel.mainCamera->front, temp);
	glm_lookat(g_rebel.mainCamera->position, temp, g_rebel.mainCamera->up, view);

	glm_translate(model, position);

	ShaderSetFloat(shaderToUse, "material.shininess", modelObject->material->shininess);

	ShaderSetInt(shaderToUse, "pointLightsCount", g_rebel.pointLightCount);

	char base[30];
	for ( unsigned int i = 0 ; i < g_rebel.pointLightCount ; i++ )
	{
		PointLight* pointLight = g_rebel.pointLights[i];

		sprintf(base, "pointLights[%u].position", i);
		ShaderSetVec3(shaderToUse, base, pointLight->position);

		sprintf(base, "pointLights[%u].ambient", i);
		ShaderSetVec3(shaderToUse, base, pointLight->light->ambient);

		sprintf(base, "pointLights[%u].diffuse", i);
		ShaderSetVec3(shaderToUse, base, pointLight->light->diffuse);

		sprintf(base, "pointLights[%u].specular", i);
		ShaderSetVec3(shaderToUse, base, pointLight->light->specular);

		sprintf(base, "pointLights[%u].constant", i);
		ShaderSetFloat(shaderToUse, base, pointLight->constant);
			
		sprintf(base, "pointLights[%u].linear", i);
		ShaderSetFloat(shaderToUse, base, pointLight->linear);

		sprintf(base, "pointLights[%u].linear", i);
		ShaderSetFloat(shaderToUse, base, pointLight->quadratic);
	}

	if ( g_rebel.directionLight != NULL )
	{
		ShaderSetVec3(shaderToUse, "directionLight.ambient", g_rebel.directionLight->light->ambient);
		ShaderSetVec3(shaderToUse, "directionLight.diffuse", g_rebel.directionLight->light->diffuse);
		ShaderSetVec3(shaderToUse, "directionLight.specular", g_rebel.directionLight->light->specular);
		ShaderSetVec3(shaderToUse, "directionLight.direction", g_rebel.directionLight->direction);
	}

	ShaderSetVec3(shaderToUse, "viewPos", g_rebel.mainCamera->position);
	ShaderSetMat4(shaderToUse, "projection", projection);
	ShaderSetMat4(shaderToUse, "view", view);
	ShaderSetMat4(shaderToUse, "model", model);

	// An inversed model is needed for a normal matrix
	// Normally, this can be done from inside the shader but is said to be a costly operation
	// So we are inversing the model using the CPU and passing it as a uniform to the shader
	// More details here under "One last thing": https://learnopengl.com/Lighting/Basic-Lighting
	mat4 inversedModel;
	glm_mat4_inv(model, inversedModel);
	ShaderSetMat4(shaderToUse, "inversedModel", inversedModel);

	ShaderSetVec3(shaderToUse, "material.color", color);

	ShaderSetInt(shaderToUse, "material.texture_diffuse1", 0);

	// TODO: Make a ModelDraw function
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, modelObject->material->textureDiffuse1);
	
	if ( modelObject->material->textureSpecular1 > 0 )
	{
		ShaderSetInt(shaderToUse, "material.texture_specular1", 1);
		glActiveTexture(GL_TEXTURE1);
		glBindTexture(GL_TEXTURE_2D, modelObject->material->textureSpecular1);
	}

	for ( unsigned int i = 0; i < modelObject->meshesSize ; i++ )
	{
		glBindVertexArray(modelObject->meshes[i]->VAO);
	
		if ( modelObject->meshes[i]->indicesSize )
			glDrawElements(GL_TRIANGLES, modelObject->meshes[i]->indicesSize, GL_UNSIGNED_INT, 0);
		else 
			glDrawArrays(GL_TRIANGLES, 0, modelObject->meshes[i]->verticesSize);
	}

	glBindVertexArray(0);
	glActiveTexture(GL_TEXTURE0);
}
