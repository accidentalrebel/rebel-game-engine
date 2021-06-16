#include "text.h"
#include <stdlib.h>
#include <assert.h>
#include "sprite.h"
#include "../core/utils.h"
#include "../external/json.h"

Text* TextCreate(char* string) {
	Text* text = (Text*)malloc(sizeof(Text));
	text->canvas = SpriteCreate(64, 64);
	text->string = malloc(strlen(string) + 1);
	strcpy(text->string, string);
	
	return text;
}

void TextLoadFont(Text* text, Font *font) {
	text->font = font;
	SpriteLoadTexture(text->canvas, text->font->fontTexture);

	char* pathWithExt = malloc(strlen(text->font->fontPath) + 4 + 1);
	strcpy(pathWithExt, text->font->fontPath);
	strcat(pathWithExt, ".fnt");
	
	char* fileContents = UtilsReadFile(pathWithExt);

	free(pathWithExt);

	struct json_value_s* root = json_parse(fileContents, strlen(fileContents));
	assert(root->type == json_type_object);
	struct json_object_s* rootObj = (struct json_object_s*)root->payload;

	// Getting the characters
	// 
	struct json_object_element_s* charsObj = rootObj->start;
	assert(strcmp(charsObj->name->string, "chars") == 0);
	struct json_value_s* charsArrayValue = charsObj->value;
	assert(charsArrayValue->type == json_type_array);
	struct json_array_s* charsArrayPayload = (struct json_array_s*)charsArrayValue->payload;

	unsigned int maxArrayCount = charsArrayPayload->length;
	
	text->font->fontChar = (FontChar**)calloc(maxArrayCount, sizeof(FontChar));

	struct json_array_element_s* charObj = charsArrayPayload->start;
	assert(charObj->value->type == json_type_object);

	for ( unsigned int i = 0; i < maxArrayCount ; i++ )
	{
		struct json_object_s* charObjE = charObj->value->payload;
		struct json_object_element_s* current = charObjE->start;
		
		text->font->fontChar[i] = (FontChar*)malloc(sizeof(FontChar));
		
		while ( current != NULL )
		{
			const char* str = ((struct json_string_s*)current->value->payload)->string;
			unsigned short value = (unsigned short)atoi(str);

			if ( strcmp(current->name->string, "id") == 0 )
				text->font->fontChar[i]->id = value;
			else if ( strcmp(current->name->string, "x") == 0 )
				text->font->fontChar[i]->x = value;
			else if ( strcmp(current->name->string, "y") == 0 )
				text->font->fontChar[i]->y = value;
			else if ( strcmp(current->name->string, "width") == 0 )
				text->font->fontChar[i]->width = value;
			else if ( strcmp(current->name->string, "height") == 0 )
				text->font->fontChar[i]->height = value;
			else if ( strcmp(current->name->string, "xadvance") == 0 )
				text->font->fontChar[i]->xAdvance = value;
			else if ( strcmp(current->name->string, "xoffset") == 0 )
				text->font->fontChar[i]->xOffset = value;
			else if ( strcmp(current->name->string, "yoffset") == 0 )
				text->font->fontChar[i]->yOffset = value;
		
			current = current->next;
		}

		charObj = charObj->next;
		if ( charObj == NULL || charObj->value == NULL )
			break;

		assert(charObj->value->type == json_type_object);
	}

	// We then populate the character map.
	// 
	unsigned int lastId = text->font->fontChar[maxArrayCount - 1]->id;
	text->font->charMap = (unsigned short*)calloc(lastId, sizeof(unsigned short));
		
	for ( unsigned int i = 0; i < maxArrayCount ; i++ )
	{
		unsigned int currentId = text->font->fontChar[i]->id;
		text->font->charMap[currentId] = i;
	}

	// Getting the common details
	//
	struct json_object_element_s* commonObj = charsObj->next;
	assert(strcmp(commonObj->name->string, "common") == 0);

	struct json_object_s* baseObj = commonObj->value->payload;
	struct json_object_element_s* base = baseObj->start; 
	base = base->next;
	assert(strcmp(base->name->string, "base") == 0);

	const char* baseChar = ((struct json_string_s*)base->value->payload)->string;
	assert(strlen(baseChar) > 0);
	unsigned short baseInt = atoi(baseChar);
	text->font->baseHeight = baseInt;
	
	free(root);
}

Font* FontLoad(const char* directory, const char* filename, char* typeName) {
	Font* font = (Font*)malloc(sizeof(Font));

	font->fontPath = malloc(strlen(directory) + strlen(filename) + 1);
	strcpy(font->fontPath, directory);
	strcat(font->fontPath, filename);
	
	char* fileNameWithExt = malloc(strlen(filename) + 4 + 1);
	strcpy(fileNameWithExt, filename);
	strcat(fileNameWithExt, ".png");
	font->fontTexture = TextureLoad(directory, fileNameWithExt, typeName);

	free(fileNameWithExt);

	return font;
}

void FontUnload(Font* font) {
	TextureUnload(font->fontTexture);
	free(font);
}

FontChar* GetFontChar(Font* font, unsigned short id)
{
	unsigned short fontCharIndex = font->charMap[id];
	return font->fontChar[fontCharIndex];
}

void TextDestroy(Text* text)
{
	free(text->string);
	free(text);
}
