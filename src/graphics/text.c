#include "text.h"
#include <stdlib.h>
#include <assert.h>
#include "sprite.h"
#include "../core/utils.h"
#include "../external/json.h"

Text* TextCreate(char* string) {
	Text* text = (Text*)malloc(sizeof(Text));
	text->canvas = SpriteCreate(27, 37);
	text->string = string;
	
	return text;
}

void TextLoadFont(Text* text, Font *font) {
	text->font = font;
	SpriteLoadTexture(text->canvas, text->font->fontTexture);

	char* fileName = "assets/fonts/font.fnt";
	char* fileContents = UtilsReadFile(fileName);

	struct json_value_s* root = json_parse(fileContents, strlen(fileContents));
	assert(root->type == json_type_object);
	struct json_object_s* rootObj = (struct json_object_s*)root->payload;
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
			int value = atoi(str);

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
	
	free(root);
}

Font* FontLoad(const char* directory, const char* filename, char* typeName) {
	Font* font = (Font*)malloc(sizeof(Font));
	font->fontTexture = TextureLoad(directory, filename, typeName);
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
