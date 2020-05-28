#include "string_utils.h"

int main(void)
{
	char output[80];
	StringInsert(output, "test", "karlo", 2);
	printf(output);
	return 0;
}
