#include "../data/structs.h"

Camera* CameraCreate();
void CameraUpdateVectors(Camera* camera);
void CameraMove(Camera *camera, enum Direction direction, float velocity);
