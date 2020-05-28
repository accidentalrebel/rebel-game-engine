#include <stdio.h>

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

