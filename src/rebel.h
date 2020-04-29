#include <GLFW/glfw3.h>

class Rebel
{
 public:
	Rebel(int windowWidth, int windowHeight);
	static bool canCloseWindow(Rebel *rebel);
	GLFWwindow *window;
 private:
};
