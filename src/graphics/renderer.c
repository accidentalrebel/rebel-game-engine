#include "renderer.h"
#include "shader.h"
#include "../rebel.h"

// This is a convenience function for creating sprites.
// It still uses the model/mesh system
Model* SpriteCreate(float width, float height)
{
	return ModelLoadFromMesh(MeshGeneratePlane(width, height));
}

void RendererDraw(Model* modelObject, vec3 position, vec3 color)
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

	for ( unsigned int i = 0 ; i < modelObject->material->loadedTexturesCount ; i++ )
	{
		Texture* texture = modelObject->material->loadedTextures[i];
		unsigned int id = texture->id;

		if ( strcmp(texture->type, "texture_diffuse" ) )
		{
			glActiveTexture(GL_TEXTURE0);
			ShaderSetInt(shaderToUse, "material.texture_diffuse1", i);
			glBindTexture(GL_TEXTURE_2D, id);
		}
		else if ( strcmp(texture->type, "texture_specular" ) )
		{
			glActiveTexture(GL_TEXTURE1);
			ShaderSetInt(shaderToUse, "material.texture_specular1", i);
			glBindTexture(GL_TEXTURE_2D, id);
		}
	}

	
	/* if ( modelObject->material->textureSpecular1 > 0 ) */
	/* { */
	/* 	ShaderSetInt(shaderToUse, "material.texture_specular1", 1); */
	/* 	glActiveTexture(GL_TEXTURE1); */
	/* 	glBindTexture(GL_TEXTURE_2D, modelObject->material->textureSpecular1); */
	/* } */

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
