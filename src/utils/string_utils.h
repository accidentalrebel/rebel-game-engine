#pragma once

// NOTE: I am aware that there are already existing libraries that does these things. I am re-implementing them here just for learning purposes.

#include <stdio.h>
#include <string.h>

void StringInsert(char *output, const char* str, const char* insert, unsigned int atIndex)
{
	unsigned int i = 0;
	while ( *str != '\0' )
	{
		if ( i < atIndex || *insert == '\0')
		{
			*output = *str;
			str++;
		}
		else if ( *insert != '\0' )
		{
			*output = *insert;
			insert++;
		}
		
		output++;
		i++;
	}
	*output = '\0';
}

void GetDirectoryFromPath(char* output, const char* str)
{
	unsigned int currentIndex = 0;
	unsigned int indexOfSlash = 0;
	while( *str != '\0' )
	{
		if ( *str == '/' )
			indexOfSlash = currentIndex;

		++currentIndex;
		++str;
	}

	str -= currentIndex;
	strncpy(output, str, indexOfSlash + 1);
}
