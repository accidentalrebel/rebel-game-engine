#include <glad/glad.h>
#include <GLFW/glfw3.h>

#include "renderer.h"
#include "../external/stb_image.h"
#include "../rebel.h"
#include "shader.h"
#include "../data/vec3.h"

Cube* CubeCreate(const char *directory, const char *filename)
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

	Cube* cube = (Cube*)malloc(sizeof(Cube));
	unsigned int attributeSizes[] = { 3, 3, 2 };	
	cube->renderObject = InitRenderObject(vertices, sizeof(vertices), 36, 8, attributeSizes, sizeof(attributeSizes));

	stbi_set_flip_vertically_on_load(true);
 	cube->renderObject->texture = LoadTextureFromFile(directory, filename);
	return cube;
}

void CubeDraw(Cube* cube, Vec3 *position, float width, float height, Vec3 *tintColor, Shader* shader)
{
	RendererDraw(cube->renderObject, position, width, height, tintColor, shader);
}

Sprite* SpriteCreate(const char *directory, const char *filename)
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

	Sprite* sprite = (Sprite*)malloc(sizeof(Sprite));
	unsigned int attributeSizes[] = { 2, 2 };
	sprite->renderObject = InitRenderObject(vertices, sizeof(vertices), 6, 4, attributeSizes, sizeof(attributeSizes));
	
	stbi_set_flip_vertically_on_load(true);
 	sprite->renderObject->texture = LoadTextureFromFile(directory, filename);
	
	return sprite;
}

void SpriteDraw(Sprite *sprite, Vec3 *position, float width, float height, Vec3 *tintColor, Shader* shader)
{
	RendererDraw(sprite->renderObject, position, width, height, tintColor, shader);
}

RenderObject* InitRenderObject(float *vertices, int verticesSize, int indicesSize, int stride, unsigned int* attributeSizes, unsigned int attributeCount)
{
	RenderObject* renderObject = (RenderObject*)malloc(sizeof(RenderObject));
	renderObject->indicesSize = indicesSize;

	glGenVertexArrays(1, &renderObject->VAO);
	glGenBuffers(1, &renderObject->VBO);
	
	glBindVertexArray(renderObject->VAO);

	glBindBuffer(GL_ARRAY_BUFFER, renderObject->VBO);
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
	
	return renderObject;
}

void RendererDraw(RenderObject *rendererObject, Vec3 *position, float width, float height, Vec3 *tintColor, Shader* shader)
{
	Shader* shaderToUse = shader;
	if ( shaderToUse == NULL )
		shaderToUse = g_rebel.defaultShader;
		
	ShaderUse(shaderToUse);
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

	temp[0] = width;
	temp[1] = height;
	temp[2] = height;
	glm_scale(model, temp);

	temp[0] = position->x / width;
	temp[1] = position->y / height;
	temp[2] = position->z;
	glm_translate(model, temp);

	temp[0] = tintColor->x;
	temp[1] = tintColor->y;
	temp[2] = tintColor->z;
	ShaderSetVec3(shaderToUse, "objectColor", temp);

	ShaderSetVec3(shaderToUse, "lightColor", (vec3){ 1.0f, 1.0f, 1.0f} );
	
	ShaderSetMat4(shaderToUse, "projection", projection);
	ShaderSetMat4(shaderToUse, "view", view);
	ShaderSetMat4(shaderToUse, "model", model);
	
	glBindVertexArray(rendererObject->VAO);
	glBindTexture(GL_TEXTURE_2D, rendererObject->texture);
	// glDrawArrays(GL_TRIANGLES, 0, 6);
	glDrawArrays(GL_TRIANGLES, 0, rendererObject->indicesSize);
}
