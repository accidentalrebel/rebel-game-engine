#include "renderer.h"
#include "shader.h"
#include "sprite.h"
#include "../rebel.h"

Renderer* RendererInit()
{
	Renderer* renderer = (Renderer*)malloc(sizeof(Renderer));
	renderer->isWireFrameMode = 0;
	return renderer;
}

void RendererDraw(Model* modelObject, vec4 drawRect, vec3 position, vec3 scale, vec3 rotation, vec4 color)
{
	RenderOptions renderOptions;
	glm_vec4_copy(drawRect, renderOptions.drawRect);
	glm_vec3_copy(position, renderOptions.position);
	glm_vec3_copy(scale, renderOptions.scale);
	glm_vec3_copy(rotation, renderOptions.rotation);
	glm_vec4_copy(color, renderOptions.color);
	glm_vec3_zero(renderOptions.viewRotation);
	glm_vec3_zero(renderOptions.pivot);

	RendererDrawEx(modelObject, renderOptions);
}

void RendererDrawEx(Model* modelObject, RenderOptions renderOptions)
{
	if ( g_rebel.renderer->isWireFrameMode )
		glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
	else
		glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
	
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
		glm_ortho(-windowWidth / size / 2, windowWidth / size / 2, -windowHeight / size / 2, windowHeight / size / 2, -100.0f, 100.0f, projection);
	}

	vec3 temp;
	glm_vec3_add(g_rebel.mainCamera->position, g_rebel.mainCamera->front, temp);
	glm_lookat(g_rebel.mainCamera->position, temp, g_rebel.mainCamera->up, view);

	glm_translate(model, renderOptions.position);

	if ( renderOptions.viewRotation[2] != 0.0f )
	{
		glm_vec3_copy((vec3){0.0f, 0.0f, renderOptions.viewRotation[2]}, temp);
		glm_rotate(view, glm_rad(renderOptions.viewRotation[2]), temp);
	}

	// Rotation
	// ========
	if ( renderOptions.rotation[0] != 0.0f )
	{
		glm_vec3_copy((vec3){renderOptions.rotation[0], 0.0f, 0.0f}, temp);
		glm_rotate(model, glm_rad(renderOptions.rotation[0]), temp);
	}

	if ( renderOptions.rotation[1] != 0.0f )
	{
		glm_vec3_copy((vec3){0.0f, renderOptions.rotation[1], 0.0f}, temp);
		glm_rotate(model, glm_rad(renderOptions.rotation[1]), temp);
	}

	if ( renderOptions.rotation[2] != 0.0f )
	{
		glm_vec3_copy((vec3){0.0f, 0.0f, renderOptions.rotation[2]}, temp);
		
		vec3 pivot;
 		pivot[0] = -renderOptions.pivot[0];
 		pivot[1] = -renderOptions.pivot[1];
		pivot[2] = 0;

		glm_rotate_at(model, pivot, glm_rad(renderOptions.rotation[2]), temp);
	}

	glm_scale(model, renderOptions.scale);

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
	ShaderSetVec4(shaderToUse, "material.color", renderOptions.color);
		
	for ( unsigned int i = 0 ; i < modelObject->material->loadedTexturesCount ; i++ )
	{
		Texture* texture = modelObject->material->loadedTextures[i];
		unsigned int id = texture->id;

		if ( strcmp(texture->type, "texture_diffuse" ) == 0 )
		{
			glActiveTexture(GL_TEXTURE0);
			ShaderSetInt(shaderToUse, "material.texture_diffuse1", i);
			glBindTexture(GL_TEXTURE_2D, id);
		}
		else if ( strcmp(texture->type, "texture_specular" ) == 0 )
		{
			glActiveTexture(GL_TEXTURE1);
			ShaderSetInt(shaderToUse, "material.texture_specular1", i);
			glBindTexture(GL_TEXTURE_2D, id);
		}

		// HANDLE DRAW RECT
		// ================
		vec4 texRect;
		
		if ( renderOptions.drawRect[0] == 0 &&
				 renderOptions.drawRect[1] == 0 &&
				 renderOptions.drawRect[2] == 0 &&
				 renderOptions.drawRect[3] == 0 )
			glm_vec4_copy((vec4){0, 0, 1, 1}, texRect);
		else
			glm_vec4_copy((vec4){renderOptions.drawRect[0] / texture->width, renderOptions.drawRect[1] / texture->height, renderOptions.drawRect[2] / texture->width, renderOptions.drawRect[3] / texture->height}, texRect);

		ShaderSetVec4(shaderToUse, "texRect", texRect);
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

void RendererDrawTextEx(Text* text, RenderOptions renderOptions)
{
	unsigned short currentXOffset = 0;
	unsigned int stringLength = strlen(text->string);
	vec3 origPosition;
	glm_vec3_copy(renderOptions.position, origPosition);
	vec3 origScale;
	glm_vec3_copy(renderOptions.scale, origScale);

	for ( unsigned int i = 0; i < stringLength; i++ )
	{
		char character = text->string[i];
		FontChar* fontChar = GetFontChar(text->font, (unsigned short)character);
	
		unsigned short textureHeight = text->font->fontTexture->height;
		unsigned short fontSize = 64;
		
		unsigned short rectX = fontChar->x;
		unsigned short rectY = fontChar->y;
		unsigned short rectWidth = fontChar->width;
		unsigned short rectHeight = fontChar->height;

		float heightScale = (float)rectHeight / fontSize;
		float widthScale = (float)rectWidth / fontSize;

		renderOptions.position[0] += currentXOffset + ((rectWidth / 2) + fontChar->xOffset) * renderOptions.scale[0] - (text->textWidth / 2);
		renderOptions.position[1] += (rectHeight / 2 * renderOptions.scale[1])         // Move the character above the line
			+ (text->font->baseHeight - rectHeight) * renderOptions.scale[1]             // Align the characters from the top
			- fontChar->yOffset * renderOptions.scale[1]                                 // Apply the offset
			- rectHeight / 2;                                                            // Move to center the anchor

		currentXOffset += fontChar->xAdvance * renderOptions.scale[0];

		renderOptions.pivot[0] = renderOptions.position[0] - origPosition[0]; // + 49.5f;
		renderOptions.pivot[1] = renderOptions.position[1] - origPosition[1]; // - (rectHeight / 2);
		static int test = false;
		if ( test < 3 )
		{
			printf("%f %f %f\n", origPosition[0], renderOptions.position[0], renderOptions.pivot[0]);
			test++;
		}
		
		renderOptions.scale[0] *= widthScale;
		renderOptions.scale[1] *= heightScale;

		glm_vec4_copy((vec4){rectX, textureHeight - rectY - rectHeight, rectWidth, rectHeight}, renderOptions.drawRect);

		RendererDrawEx(text->canvas, renderOptions);
		
		glm_vec3_copy(origPosition, renderOptions.position);
		glm_vec3_copy(origScale, renderOptions.scale);
	}
}

void RendererDrawText(Text* text, vec3 position, vec3 scale, vec3 rotation, vec3 viewRotation, vec4 color)
{
	RenderOptions renderOptions;
	glm_vec3_copy(position, renderOptions.position);
	glm_vec3_copy(scale, renderOptions.scale);
	glm_vec3_copy(rotation, renderOptions.rotation);
	glm_vec3_copy(viewRotation, renderOptions.viewRotation);
	glm_vec4_copy(color, renderOptions.color);
	
	RendererDrawTextEx(text, renderOptions);
}

bool RendererIsWireFrameEnabled()
{
	return g_rebel.renderer->isWireFrameMode;
}

void RendererSetWireFrameMode(bool status)
{
	g_rebel.renderer->isWireFrameMode = status;
}
