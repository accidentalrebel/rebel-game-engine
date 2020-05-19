PLATFORM = windows
CC = g++
CSC = csc
PREDEFINES =

INCLUDE_FLAGS = -Isrc/external -Isrc/external/glad/include -Isrc/external/glfw/include -I/usr/include 
LINKER_FLAGS = -L../libs/ -L -lrebel -L/usr/lib

ifeq ($(PLATFORM),linux)
	CSC = chicken-csc
	LINKER_FLAGS += -L/usr/lib -L -lglfw
else ifeq ($(PLATFORM),macosx)
	LINKER_FLAGS += -L/usr/local/lib -L -lglfw
	PREDEFINES += -D PLATFORM=$(PLATFORM)
else ifeq ($(PLATFORM),windows)
	LINKER_FLAGS += -L/c/msys64/mingw64/lib -L -lglfw3
endif

all:	run

build:	lib

run:
	cd src ; $(CSC) ../scripts/main.scm -o ../output/game $(LINKER_FLAGS) -debug F -c++ -static
	output/game

lib: 	objs
	ar rvs libs/librebel.a tmp/rebel.o tmp/vec3.o tmp/window.o tmp/stub.o tmp/shader.o tmp/camera.o tmp/renderer.o tmp/glad.o tmp/keyboard.o tmp/mouse.o

objs:
	gcc -c src/external/glad/src/glad.c $(INCLUDE_FLAGS) -o tmp/glad.o
	g++ -c src/data/vec3.c $(INCLUDE_FLAGS) -o tmp/vec3.o
	gcc -c src/rebel.cpp $(INCLUDE_FLAGS) -o tmp/rebel.o
	gcc -c src/graphics/shader.cpp $(INCLUDE_FLAGS) -o tmp/shader.o
	g++ -c src/graphics/renderer.c $(INCLUDE_FLAGS) -o tmp/renderer.o
	gcc -c src/graphics/camera.cpp $(INCLUDE_FLAGS) -o tmp/camera.o
	gcc -c src/input/keyboard.cpp $(INCLUDE_FLAGS) -o tmp/keyboard.o
	g++ -c src/input/mouse.c $(INCLUDE_FLAGS) -o tmp/mouse.o
	g++ -c src/core/window.c $(INCLUDE_FLAGS) $(PREDEFINES) -o tmp/window.o

clean:
	rm ./tmp/*.*
	rm ./libs/*.*
