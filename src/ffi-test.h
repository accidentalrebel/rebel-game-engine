typedef struct Vector3 {
	float x;
	float y;
	float z;
} Vector3;

void FFITest();
void StringAccept(char* v);
void Vector3Accept(Vector3 v);
Vector3* MakeVector3(float x, float y, float z);
