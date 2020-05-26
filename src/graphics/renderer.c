#include <glad/glad.h>
#include <GLFW/glfw3.h>

#include "renderer.h"
#include "material.h"
#include "../external/stb_image.h"
#include "../rebel.h"
#include "shader.h"
#include "../data/vec3.h"

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

void RendererDraw(Renderer *rendererObject, Vec3 *position, float width, float height, Vec3 *tintColor)
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

	vec3 cameraPos;
	vec3 cameraFront;
	vec3 cameraUp;
	Vec3ToGlm(g_rebel.mainCamera->position, cameraPos);
	Vec3ToGlm(g_rebel.mainCamera->front, cameraFront);
	Vec3ToGlm(g_rebel.mainCamera->up, cameraUp);
	
	vec3 temp;
	glm_vec3_add(cameraPos, cameraFront, temp);
	glm_lookat(cameraPos, temp, cameraUp, view);

	glm_scale(model, (vec3){ width, height, height });

	glm_translate(model, (vec3) { position->x / width, position->y / height, position->z});

	ShaderSetFloat(shaderToUse, "material.shininess", rendererObject->material->shininess);

	ShaderSetVec3(shaderToUse, "pointLights[0].position", (vec3){ 1.25f, 0.0f, -1.0f });
	ShaderSetVec3(shaderToUse, "pointLights[0].ambient", (vec3){ 0.05f, 0.05f, 0.05f });
	ShaderSetVec3(shaderToUse, "pointLights[0].diffuse", (vec3){ 0.8f, 0.2f, 0.2f });
	ShaderSetVec3(shaderToUse, "pointLights[0].specular", (vec3){ 1.0f, 1.0f, 1.0f });
	ShaderSetFloat(shaderToUse, "pointLights[0].constant", 1.0f);
	ShaderSetFloat(shaderToUse, "pointLights[0].linear", 0.09f);
	ShaderSetFloat(shaderToUse, "pointLights[0].quadratic", 0.032f);

	if ( g_rebel.directionLight != NULL )
	{
		Vec3ToGlm(g_rebel.directionLight->light->ambient, temp);
		ShaderSetVec3(shaderToUse, "directionLight.ambient", temp);

		Vec3ToGlm(g_rebel.directionLight->light->diffuse, temp);
		ShaderSetVec3(shaderToUse, "directionLight.diffuse", temp);

		Vec3ToGlm(g_rebel.directionLight->light->specular, temp);
		ShaderSetVec3(shaderToUse, "directionLight.specular", temp);

		Vec3ToGlm(g_rebel.directionLight->direction, temp);
		ShaderSetVec3(shaderToUse, "directionLight.direction", temp);
	}
	else
	{
		// TODO: The renderer should still work even without a direction light
		printf("ERROR::RENDERER::THERE IS NO EXISTING DIRECTION LIGHT. CREATE ONE USING DirectionLightCreate.\n");
	}

	ShaderSetVec3(shaderToUse, "viewPos", cameraPos);
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
