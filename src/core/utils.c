#include "utils.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

char* UtilsReadFile(const char* filename) {
  FILE* fp = fopen(filename, "rb");
	if ( fp == NULL )
	{
		printf("ERROR::UTILS::UTILS_READ_FILE::FILE_READ_ERROR %s\n", filename);
		return NULL;
	}

  fseek(fp, 0, SEEK_END);
  long int size = ftell(fp);
  rewind(fp);

  char* result = (char*)malloc(size+1);

  int index = 0;
  int c;
  while ((c = fgetc(fp)) != EOF) {
    result[index] = c;
    index++;
  }

	result[index] = '\0';

  fclose(fp);

	return result;
}
