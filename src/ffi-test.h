#define ORTHOGRAPHIC 0
#define PERSPECTIVE 1

typedef struct Vector3 {
	float x;
	float y;
	float z;
} Vector3;

void FFITest();
void StringAccept(char* v);
void Vector3Accept(Vector3 v);
void SetCameraProjection(int projection);
