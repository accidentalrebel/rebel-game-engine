#include <glad/glad.h>
#include <GLFW/glfw3.h>

#include "renderer.h"
#include "material.h"
#include "../external/stb_image.h"
#include "../rebel.h"
#include "shader.h"
#include "../data/vec3.h"

Renderer* CubeCreate(const char *directory, const char *filename)
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
 	renderer->texture = LoadTextureFromFile(directory, filename);
	return renderer;
}

Renderer* SpriteCreate(const char *directory, const char *filename)
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
 	renderer->texture = LoadTextureFromFile(directory, filename);
	
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

// Note: Sets the color of the renderer. Overwrites the material by resseting the material to default values.
// If you more control over the material, manipulate the values directly
void RendererSetColor(Renderer* renderObject, Vec3* color)
{
	renderObject->material->ambient = color;
	Vec3Set(renderObject->material->diffuse, 0.0f, 0.0f, 0.0f);
	Vec3Set(renderObject->material->specular, 0.0f, 0.0f, 0.0f);
	renderObject->material->shininess = 1.0f;
}

void RendererDraw(Renderer *rendererObject, Vec3 *position, float width, float height, Vec3 *tintColor)
{
	Shader* shaderToUse;
	if ( g_rebel.currentShader != NULL )
		shaderToUse = g_rebel.currentShader;
	else
		shaderToUse = g_rebel.defaultShader;
		
	ShaderSetInt(shaderToUse, "texture1", 0);

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

	/* temp[0] = tintColor->x; */
	/* temp[1] = tintColor->y; */
	/* temp[2] = tintColor->z; */
	vec3 ambient;
	vec3 diffuse;
	vec3 specular;
	Vec3ToGlm(rendererObject->material->ambient, ambient);
	Vec3ToGlm(rendererObject->material->diffuse, diffuse);
	Vec3ToGlm(rendererObject->material->specular, specular);
		
	ShaderSetVec3(shaderToUse, "material.ambient", ambient);
	ShaderSetVec3(shaderToUse, "material.diffuse", diffuse);
	ShaderSetVec3(shaderToUse, "material.specular", specular);
	ShaderSetFloat(shaderToUse, "material.shininess", rendererObject->material->shininess);

	if ( g_rebel.directionLight != NULL )
	{
		Vec3ToGlm(g_rebel.directionLight->color, temp);
		ShaderSetVec3(shaderToUse, "directionLight.ambient", (vec3){ 1.0f, 0.0f, 0.0f});
		ShaderSetVec3(shaderToUse, "directionLight.diffuse", (vec3){ 1.0f, 0.0f, 0.0f});
		ShaderSetVec3(shaderToUse, "directionLight.specular", (vec3){ 1.0f, 0.5f, 0.5f});

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
	glBindTexture(GL_TEXTURE_2D, rendererObject->texture);
	// glDrawArrays(GL_TRIANGLES, 0, 6);
	glDrawArrays(GL_TRIANGLES, 0, rendererObject->indicesSize);
}
