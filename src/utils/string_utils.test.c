#include "string_utils.h"

int main(void)
{
	char output[80];
	StringInsert(output, "test", "karlo", 2);
	printf("Output is: %s\n", output);

	GetDirectoryFromPath(output, "test/test2/test3.png");
	printf("Directory is: %s\n", output);
	
	return 0;
}
