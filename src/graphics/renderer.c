#include <glad/glad.h>
#include <GLFW/glfw3.h>
#include <string.h>

#include "renderer.h"
#include "material.h"
#include "../external/stb_image.h"
#include "../rebel.h"
#include "shader.h"

Renderer* CubeCreate()
{
	float vertices[] = {
    -0.5f, -0.5f, -0.5f,  0.0f,  0.0f, -1.0f,  0.0f, 0.0f,
		0.5f, -0.5f, -0.5f,  0.0f,  0.0f, -1.0f,   1.0f, 0.0f,
		0.5f,  0.5f, -0.5f,  0.0f,  0.0f, -1.0f,   1.0f, 1.0f,
		0.5f,  0.5f, -0.5f,  0.0f,  0.0f, -1.0f,   1.0f, 1.0f,
    -0.5f,  0.5f, -0.5f,  0.0f,  0.0f, -1.0f,   0.0f, 1.0f,
    -0.5f, -0.5f, -0.5f,  0.0f,  0.0f, -1.0f,   0.0f, 0.0f,

    -0.5f, -0.5f,  0.5f,  0.0f,  0.0f, 1.0f,  0.0f, 0.0f,
		0.5f, -0.5f,  0.5f,  0.0f,  0.0f, 1.0f,  1.0f, 0.0f,
		0.5f,  0.5f,  0.5f,  0.0f,  0.0f, 1.0f,  1.0f, 1.0f,
		0.5f,  0.5f,  0.5f,  0.0f,  0.0f, 1.0f,  1.0f, 1.0f,
    -0.5f,  0.5f,  0.5f,  0.0f,  0.0f, 1.0f,  0.0f, 1.0f,
    -0.5f, -0.5f,  0.5f,  0.0f,  0.0f, 1.0f,  0.0f, 0.0f,

    -0.5f,  0.5f,  0.5f, -1.0f,  0.0f,  0.0f,  1.0f, 0.0f,
    -0.5f,  0.5f, -0.5f, -1.0f,  0.0f,  0.0f,  1.0f, 1.0f,
    -0.5f, -0.5f, -0.5f, -1.0f,  0.0f,  0.0f,  0.0f, 1.0f,
    -0.5f, -0.5f, -0.5f, -1.0f,  0.0f,  0.0f,  0.0f, 1.0f,
    -0.5f, -0.5f,  0.5f, -1.0f,  0.0f,  0.0f,  0.0f, 0.0f,
    -0.5f,  0.5f,  0.5f, -1.0f,  0.0f,  0.0f,  1.0f, 0.0f,

		0.5f,  0.5f,  0.5f,  1.0f,  0.0f,  0.0f,  1.0f, 0.0f,
		0.5f,  0.5f, -0.5f,  1.0f,  0.0f,  0.0f,  1.0f, 1.0f,
		0.5f, -0.5f, -0.5f,  1.0f,  0.0f,  0.0f,  0.0f, 1.0f,
		0.5f, -0.5f, -0.5f,  1.0f,  0.0f,  0.0f,  0.0f, 1.0f,
		0.5f, -0.5f,  0.5f,  1.0f,  0.0f,  0.0f,  0.0f, 0.0f,
		0.5f,  0.5f,  0.5f,  1.0f,  0.0f,  0.0f,  1.0f, 0.0f,

    -0.5f, -0.5f, -0.5f,  0.0f, -1.0f,  0.0f,  0.0f, 1.0f,
		0.5f, -0.5f, -0.5f,  0.0f, -1.0f,  0.0f,  1.0f, 1.0f,
		0.5f, -0.5f,  0.5f,  0.0f, -1.0f,  0.0f,  1.0f, 0.0f,
		0.5f, -0.5f,  0.5f,  0.0f, -1.0f,  0.0f,  1.0f, 0.0f,
    -0.5f, -0.5f,  0.5f,  0.0f, -1.0f,  0.0f,  0.0f, 0.0f,
    -0.5f, -0.5f, -0.5f,  0.0f, -1.0f,  0.0f,  0.0f, 1.0f,

    -0.5f,  0.5f, -0.5f,  0.0f,  1.0f,  0.0f,  0.0f, 1.0f,
		0.5f,  0.5f, -0.5f,  0.0f,  1.0f,  0.0f,  1.0f, 1.0f,
		0.5f,  0.5f,  0.5f,  0.0f,  1.0f,  0.0f,  1.0f, 0.0f,
		0.5f,  0.5f,  0.5f,  0.0f,  1.0f,  0.0f,  1.0f, 0.0f,
    -0.5f,  0.5f,  0.5f,  0.0f,  1.0f,  0.0f,  0.0f, 0.0f,
    -0.5f,  0.5f, -0.5f,  0.0f,  1.0f,  0.0f, 0.0f, 1.0f
	}; 

	unsigned int attributeSizes[] = { 3, 3, 2 };	
	Renderer* renderer = RendererCreate(vertices, sizeof(vertices), 36, 8, attributeSizes, sizeof(attributeSizes));

	stbi_set_flip_vertically_on_load(true);
	return renderer;
}

Renderer* SpriteCreate()
{
	float vertices[] = {  
		// positions   // texCoords
		-0.5f,  0.5f,  0.0f, 1.0f,
		-0.5f, -0.5f,  0.0f, 0.0f,
		0.5f, -0.5f,  1.0f, 0.0f,

		-0.5f,  0.5f,  0.0f, 1.0f,
		0.5f, -0.5f,  1.0f, 0.0f,
		0.5f,  0.5f,  1.0f, 1.0f
	};

	unsigned int attributeSizes[] = { 2, 2 };
	Renderer* renderer = RendererCreate(vertices, sizeof(vertices), 6, 4, attributeSizes, sizeof(attributeSizes));
	
	stbi_set_flip_vertically_on_load(true);
	
	return renderer;
}

Renderer* RendererCreate(float *vertices, int verticesSize, int indicesSize, int stride, unsigned int* attributeSizes, unsigned int attributeCount)
{
	Renderer* renderer = (Renderer*)malloc(sizeof(Renderer));
	renderer->indicesSize = indicesSize;
	renderer->material = MaterialCreate();
	
	glGenVertexArrays(1, &renderer->VAO);
	glGenBuffers(1, &renderer->VBO);
	
	glBindVertexArray(renderer->VAO);

	glBindBuffer(GL_ARRAY_BUFFER, renderer->VBO);
	glBufferData(GL_ARRAY_BUFFER, verticesSize, vertices, GL_STATIC_DRAW);

	unsigned int pointerPos = 0;
 	for ( unsigned int i = 0 ; i < attributeCount ; i++ )
	{
		void* p = 0;
		if ( i > 0 )
		{
			pointerPos += attributeSizes[i-1];
			p = (void*)(pointerPos * sizeof(float));
		}

		glVertexAttribPointer(i, attributeSizes[i], GL_FLOAT, GL_FALSE, stride * sizeof(float), p);
		glEnableVertexAttribArray(i);
	}

	glBindVertexArray(0);
	
	return renderer;
}

Renderer* RendererCreate2(Mesh* mesh, int verticesSize, int indicesSize, int stride, unsigned int* attributeSizes, unsigned int attributeCount)
{
	Renderer* renderer = (Renderer*)malloc(sizeof(Renderer));
	renderer->indicesSize = indicesSize;
	renderer->material = MaterialCreate();
	
	glGenVertexArrays(1, &mesh->VAO);
	glGenBuffers(1, &mesh->VBO);
	
	glBindVertexArray(mesh->VAO);

	glBindBuffer(GL_ARRAY_BUFFER, mesh->VBO);
	glBufferData(GL_ARRAY_BUFFER, verticesSize, mesh->vertices[0], GL_STATIC_DRAW);

	unsigned int pointerPos = 0;
 	for ( unsigned int i = 0 ; i < attributeCount ; i++ )
	{
		void* p = 0;
		if ( i > 0 )
		{
			pointerPos += attributeSizes[i-1];
			p = (void*)(pointerPos * sizeof(float));
		}

		glVertexAttribPointer(i, attributeSizes[i], GL_FLOAT, GL_FALSE, stride * sizeof(float), p);
		glEnableVertexAttribArray(i);
	}

	glBindVertexArray(0);
	
	return renderer;
}

void RendererDraw(Renderer *rendererObject, vec3 position, float width, float height)
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

	glm_scale(model, (vec3){ width, height, height });

	glm_translate(model, (vec3) { position[0] / width, position[1] / height, position[2] / height});

	ShaderSetFloat(shaderToUse, "material.shininess", rendererObject->material->shininess);

	ShaderSetInt(shaderToUse, "pointLightsCount", g_rebel.pointLightCount);

	char base[30];
	for ( unsigned int i = 0 ; i < g_rebel.pointLightCount ; i++ )
	{
		PointLight* pointLight = g_rebel.pointLights[i];

		sprintf(base, "pointLights[%u].position", i);
		ShaderSetVec3(shaderToUse, base, pointLight->position);

		Vec3ToGlm(pointLight->light->ambient, temp);
		sprintf(base, "pointLights[%u].ambient", i);
		ShaderSetVec3(shaderToUse, base, temp);

		Vec3ToGlm(pointLight->light->diffuse, temp);
		sprintf(base, "pointLights[%u].diffuse", i);
		ShaderSetVec3(shaderToUse, base, temp);

		Vec3ToGlm(pointLight->light->specular, temp);
		sprintf(base, "pointLights[%u].specular", i);
		ShaderSetVec3(shaderToUse, base, temp);

		sprintf(base, "pointLights[%u].constant", i);
		ShaderSetFloat(shaderToUse, base, pointLight->constant);
			
		sprintf(base, "pointLights[%u].linear", i);
		ShaderSetFloat(shaderToUse, base, pointLight->linear);

		sprintf(base, "pointLights[%u].linear", i);
		ShaderSetFloat(shaderToUse, base, pointLight->quadratic);
	}

	if ( g_rebel.directionLight != NULL )
	{
		Vec3ToGlm(g_rebel.directionLight->light->ambient, temp);
		ShaderSetVec3(shaderToUse, "directionLight.ambient", temp);

		Vec3ToGlm(g_rebel.directionLight->light->diffuse, temp);
		ShaderSetVec3(shaderToUse, "directionLight.diffuse", temp);

		Vec3ToGlm(g_rebel.directionLight->light->specular, temp);
		ShaderSetVec3(shaderToUse, "directionLight.specular", temp);

		ShaderSetVec3(shaderToUse, "directionLight.direction", g_rebel.directionLight->direction);
	}
	else
	{
		// TODO: The renderer should still work even without a direction light
		printf("ERROR::RENDERER::THERE IS NO EXISTING DIRECTION LIGHT. CREATE ONE USING DirectionLightCreate.\n");
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
	
	glBindVertexArray(rendererObject->VAO);

	if ( rendererObject->material->color != NULL )
		ShaderSetVec3(shaderToUse, "material.color", rendererObject->material->color);

	ShaderSetInt(shaderToUse, "material.texture_diffuse1", 0);
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, rendererObject->material->textureDiffuse1);
	
	if ( rendererObject->material->textureSpecular1 > 0 )
	{
		ShaderSetInt(shaderToUse, "material.texture_specular1", 1);
		glActiveTexture(GL_TEXTURE1);
		glBindTexture(GL_TEXTURE_2D, rendererObject->material->textureSpecular1);
	}

	glDrawArrays(GL_TRIANGLES, 0, rendererObject->indicesSize);
}

void RendererDraw2(Model* modelObject, Vec3 *position, float width, float height)
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

	glm_scale(model, (vec3){ width, height, height });

	glm_translate(model, (vec3) { position->x / width, position->y / height, position->z / height});

	ShaderSetFloat(shaderToUse, "material.shininess", modelObject->material->shininess);

	ShaderSetInt(shaderToUse, "pointLightsCount", g_rebel.pointLightCount);

	char base[30];
	for ( unsigned int i = 0 ; i < g_rebel.pointLightCount ; i++ )
	{
		PointLight* pointLight = g_rebel.pointLights[i];

		sprintf(base, "pointLights[%u].position", i);
		ShaderSetVec3(shaderToUse, base, pointLight->position);

		Vec3ToGlm(pointLight->light->ambient, temp);
		sprintf(base, "pointLights[%u].ambient", i);
		ShaderSetVec3(shaderToUse, base, temp);

		Vec3ToGlm(pointLight->light->diffuse, temp);
		sprintf(base, "pointLights[%u].diffuse", i);
		ShaderSetVec3(shaderToUse, base, temp);

		Vec3ToGlm(pointLight->light->specular, temp);
		sprintf(base, "pointLights[%u].specular", i);
		ShaderSetVec3(shaderToUse, base, temp);

		sprintf(base, "pointLights[%u].constant", i);
		ShaderSetFloat(shaderToUse, base, pointLight->constant);
			
		sprintf(base, "pointLights[%u].linear", i);
		ShaderSetFloat(shaderToUse, base, pointLight->linear);

		sprintf(base, "pointLights[%u].linear", i);
		ShaderSetFloat(shaderToUse, base, pointLight->quadratic);
	}

	if ( g_rebel.directionLight != NULL )
	{
		Vec3ToGlm(g_rebel.directionLight->light->ambient, temp);
		ShaderSetVec3(shaderToUse, "directionLight.ambient", temp);

		Vec3ToGlm(g_rebel.directionLight->light->diffuse, temp);
		ShaderSetVec3(shaderToUse, "directionLight.diffuse", temp);

		Vec3ToGlm(g_rebel.directionLight->light->specular, temp);
		ShaderSetVec3(shaderToUse, "directionLight.specular", temp);

		ShaderSetVec3(shaderToUse, "directionLight.direction", g_rebel.directionLight->direction);
	}
	else
	{
		// TODO: The renderer should still work even without a direction light
		printf("ERROR::RENDERER::THERE IS NO EXISTING DIRECTION LIGHT. CREATE ONE USING DirectionLightCreate.\n");
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
	
	glBindVertexArray(modelObject->meshes[0]->VAO);

	if ( modelObject->material->color != NULL )
		ShaderSetVec3(shaderToUse, "material.color", modelObject->material->color);

	ShaderSetInt(shaderToUse, "material.texture_diffuse1", 0);
	glActiveTexture(GL_TEXTURE0);
	glBindTexture(GL_TEXTURE_2D, modelObject->material->textureDiffuse1);
	
	if ( modelObject->material->textureSpecular1 > 0 )
	{
		ShaderSetInt(shaderToUse, "material.texture_specular1", 1);
		glActiveTexture(GL_TEXTURE1);
		glBindTexture(GL_TEXTURE_2D, modelObject->material->textureSpecular1);
	}

	glDrawArrays(GL_TRIANGLES, 0, modelObject->meshes[0]->verticesSize);

	free(position);
}
