#include "texture.h"
#include <stdlib.h>
#include <string.h>
#include "shader.h"

Texture* TextureLoad(const char* directory, const char* fileName, char* typeName)
{
	Texture* texture = (Texture*)malloc(sizeof(Texture));
	int width, height;
	char fullPath[100] = "";
	strcat(fullPath, directory);
	strcat(fullPath, fileName);

	glGenTextures(1, &texture->id);

	int nrComponents;
	unsigned char *data = stbi_load(fullPath, &width, &height, &nrComponents, 0);
	if (data)
	{
		texture->width = (float)width;
		texture->height = (float)height;
		
		GLenum format;
		if (nrComponents == 1)
			format = GL_RED;
		else if (nrComponents == 3)
			format = GL_RGB;
		else if (nrComponents == 4)
			format = GL_RGBA;

		glBindTexture(GL_TEXTURE_2D, texture->id);
		glTexImage2D(GL_TEXTURE_2D, 0, format, width, height, 0, format, GL_UNSIGNED_BYTE, data);
		glGenerateMipmap(GL_TEXTURE_2D);

		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, format == GL_RGBA ? GL_CLAMP_TO_EDGE : GL_REPEAT);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, format == GL_RGBA ? GL_CLAMP_TO_EDGE : GL_REPEAT);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
		glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

		stbi_image_free(data);
	}
	else
	{
		printf("ERROR::SHADER::Texture failed to load at path: %s\n", fullPath);
		stbi_image_free(data);
	}
	
	texture->type = typeName;
	texture->path = malloc(strlen(fileName)+1);
	strcpy(texture->path, fileName);

	return texture;
}

void TextureUnload(Texture* texture)
{
	if ( texture->id > 0 )
		glDeleteTextures(1, &texture->id);

	free(texture);
}
