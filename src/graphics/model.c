#include "model.h"
#include "mesh.h"
#include "material.h"
#include "shader.h"
#include "../rebel.h"

Model* ModelLoadFromMesh(Mesh* mesh)
{
	Model* model = (Model*)malloc(sizeof(Model));
	model->meshes = (Mesh**)calloc(1, sizeof(Mesh));
	model->meshes[0] = mesh;
	model->material = MaterialCreate();
	return model;
}

Model* ModelLoad(const char* path)
{
	Model* model = (Model*)malloc(sizeof(Model));
	model->meshes = (Mesh**)calloc(1, sizeof(Mesh));
	model->material = MaterialCreate();
	
	const struct aiScene* scene = aiImportFile(path, aiProcess_Triangulate | aiProcess_FlipUVs);
	if ( !scene || scene->mFlags & AI_SCENE_FLAGS_INCOMPLETE || !scene->mRootNode )
	{
		printf("ERROR::ASSIMP::%s\n", aiGetErrorString());
		return NULL;
	}

	unsigned int currentMeshIndex = 0;
	ModelProcessNode(model, scene->mRootNode, scene, &currentMeshIndex);

	return model;
}

void ModelProcessNode(Model* model, const struct aiNode* node, const struct aiScene* scene, unsigned int *currentMeshIndex)
{
	printf("NUM MESHES: %i\n", node->mNumMeshes);
	for(unsigned int i = 0; i < node->mNumMeshes; i++)
	{
		printf("Processing mesh at %i\n", i);
		struct aiMesh *mesh = scene->mMeshes[node->mMeshes[i]];
		Mesh* m = ModelProcessMesh(mesh, scene);
		MeshSetup(m);
		printf("Adding to index %i\n", *currentMeshIndex);
		model->meshes[*currentMeshIndex] = m;
		printf("VAO is now %i\n", m->VAO);
		(*currentMeshIndex)++;
	}
	for(unsigned int i = 0; i < node->mNumChildren; i++)
	{
		printf("RECURSING...\n");
		ModelProcessNode(model, node->mChildren[i], scene, currentMeshIndex);
	}
}

Mesh* ModelProcessMesh(const struct aiMesh* mesh, const struct aiScene* scene)
{
	Mesh* m = MeshCreate();
	m->verticesSize = mesh->mNumVertices;

	printf(">>>>>> Num faces: %i\n", mesh->mNumFaces);
	m->indicesSize = mesh->mNumFaces * 3;

	m->vertices = (Vertex**)calloc(m->verticesSize, sizeof(Vertex));
	m->indices = (unsigned int*)calloc(m->indicesSize, sizeof(unsigned int));

	for(unsigned int i = 0; i < m->verticesSize; i++)
	{
		m->vertices[i] = (Vertex*)malloc(sizeof(Vertex));

		glm_vec3_copy((vec3) { mesh->mVertices[i].x, mesh->mVertices[i].y, mesh->mVertices[i].z }, m->vertices[i]->position);
		glm_vec3_copy((vec3) { mesh->mNormals[i].x, mesh->mNormals[i].y, mesh->mNormals[i].z }, m->vertices[i]->normal);

		if ( mesh->mTextureCoords[0])
			glm_vec2_copy((vec3){mesh->mTextureCoords[0][i].x, mesh->mTextureCoords[0][i].y}, m->vertices[i]->texCoords);
		else
			glm_vec2_copy((vec3){0, 0}, m->vertices[i]->texCoords);

		printf(">>>>>> At index: %i\n", i);
		printf(">>>>>> %f,%f,%f\n", m->vertices[i]->position[0], m->vertices[i]->position[1], m->vertices[i]->position[2]);
		printf(">>>>>> %f,%f,%f\n", m->vertices[i]->normal[0], m->vertices[i]->normal[1], m->vertices[i]->normal[2]);
		printf(">>>>>> %f,%f\n\n", m->vertices[i]->texCoords[0], m->vertices[i]->texCoords[1]);
	}

	for(unsigned int i = 0; i < mesh->mNumFaces; i++)
	{
		struct aiFace face = mesh->mFaces[i];
		printf("NUM INDICES: %i\n", face.mNumIndices);
		for(unsigned int j = 0; j < face.mNumIndices; j++)
		{
			printf("INDEX %i: ", (i * 3) + j);
			m->indices[(i * 3) + j] = face.mIndices[j];
			printf("%i\n", m->indices[(i * 3) + j]);
		}
	}
	printf("END");
	return m;
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
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, modelObject->material->textureDiffuse1);
	
	if ( modelObject->material->textureSpecular1 > 0 )
	{
		ShaderSetInt(shaderToUse, "material.texture_specular1", 1);
		glActiveTexture(GL_TEXTURE1);
		glBindTexture(GL_TEXTURE_2D, modelObject->material->textureSpecular1);
	}

	glBindVertexArray(modelObject->meshes[0]->VAO);
	
	if ( modelObject->meshes[0]->indicesSize )
		glDrawElements(GL_TRIANGLES, modelObject->meshes[0]->indicesSize, GL_UNSIGNED_INT, 0);
	else 
		glDrawArrays(GL_TRIANGLES, 0, modelObject->meshes[0]->verticesSize);

	glBindVertexArray(0);
}
