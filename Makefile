INCLUDE_FLAGS = -I/home/arebel/development/tools/glad/include -I/home/arebel/development/tools/glfw/include

LINKER_FLAGS = -lGL -lX11 -lpthread -ldl -lglfw3

all:	run
build:	lib

run:
	cd src ; chicken-csc ../scripts/main.scm -o ../output/game -L../libs/ -L -lrebel -L/usr/lib/ -L -lX11 -L -lpthread -L -ldl -L -lglfw3 -debug F -c++
	output/game

lib: 	objs
	ar rvs libs/librebel.a tmp/rebel.o tmp/window.o tmp/stub.o tmp/shader.o tmp/sprite.o tmp/glad.o tmp/keyboard.o

objs:
	g++ -c src/external/glad.c $(INCLUDE_FLAGS) $(LINKER_FLAGS) -o tmp/glad.o
	g++ -c src/rebel.cpp $(INCLUDE_FLAGS) $(LINKER_FLAGS) -o tmp/rebel.o
	g++ -c src/graphics/shader.cpp $(INCLUDE_FLAGS) $(LINKER_FLAGS) -o tmp/shader.o
	g++ -c src/graphics/sprite.cpp $(INCLUDE_FLAGS) $(LINKER_FLAGS) -o tmp/sprite.o
	g++ -c src/input/keyboard.cpp $(INCLUDE_FLAGS) $(LINKER_FLAGS) -o tmp/keyboard.o
	g++ -c src/core/window.cpp $(INCLUDE_FLAGS) $(LINKER_FLAGS) -o tmp/window.o
	g++ -c src/stub.cpp $(INCLUDE_FLAGS) $(LINKER_FLAGS) -o tmp/stub.o
