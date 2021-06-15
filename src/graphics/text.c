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
	
	text->font->fontChar = (FontChar*)malloc(sizeof(FontChar));

	char* fileName = "assets/fonts/font.fnt";
	char* fileContents = UtilsReadFile(fileName);
	printf("################ %s", fileContents);

	struct json_value_s* root = json_parse(fileContents, strlen(fileContents));
	assert(root->type == json_type_object);
	
	struct json_object_s* rootObj = (struct json_object_s*)root->payload;
	printf("########### %d\n", rootObj->length);

	struct json_object_element_s* charsObj = rootObj->start;
	printf("########### %s\n", charsObj->name->string);
	assert(strcmp(charsObj->name->string, "chars") == 0);

	struct json_value_s* chars_array_value = charsObj->value;
	assert(chars_array_value->type == json_type_array);

	struct json_array_s* charsArray = (struct json_array_s*)chars_array_value->payload;

	struct json_array_element_s* charObj = charsArray->start;
	assert(charObj->value->type == json_type_object);

	struct json_object_s* charObjE = charObj->value->payload;

	struct json_object_element_s* current = charObjE->start;

	for ( int i = 0; i < (int)charsArray->length ; i++ )
	{
		while ( current != NULL )
		{
			printf("########### %s\n", current->name->string);
			if ( strcmp(current->name->string, "id") == 0 )
			{
				printf("########### FOUND ######## %s\n", ((struct json_string_s*)current->value->payload)->string);
			}
		
			current = current->next;
		}
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
