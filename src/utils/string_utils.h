#include <stdio.h>

void StringInsert(const char* str, const char* insert, unsigned int atIndex)
{
	char output[80];

	unsigned int i = 0;
	while ( *str != '\0' )
	{
		if ( i < atIndex || *insert == '\0')
		{
			output[i] = *str;
			str++;
		}
		else if ( *insert != '\0' )
		{
			output[i] = *insert;
			insert++;
		}
		
		printf("%c", output[i]);
		
		i++;
	}
	output[i] = '\0';

	printf("\n%s", output);
}

