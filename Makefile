PLATFORM = linux
CC = g++
CHICKEN_CSC = csc

INCLUDE_FLAGS = -Isrc/external/glad/include -Isrc/external/glfw/include

LINKER_FLAGS = -L../libs/ -L -lrebel -L/usr/lib/ -L -lglfw3

ifeq ($(PLATFORM),linux)
	CHICKEN_CSC = chicken-csc # In arch, csc is renamed to chicken-csc
	LINKER_FLAGS += -L/usr/lib -L -lX11 -L -lpthread 
endif

# Check if really needed
# LINKER_FLAGS += -L -ldl

all:	run

build:	lib

run:
	cd src ; chicken-csc ../scripts/main.scm -o ../output/game $(LINKER_FLAGS) -debug F -c++ -static
	output/game

lib: 	objs
	ar rvs libs/librebel.a tmp/rebel.o tmp/window.o tmp/stub.o tmp/shader.o tmp/sprite.o tmp/glad.o tmp/keyboard.o

objs:
	$(CC) -c src/external/glad/src/glad.c $(INCLUDE_FLAGS) -o tmp/glad.o
	$(CC) -c src/rebel.cpp $(INCLUDE_FLAGS) -o tmp/rebel.o
	$(CC) -c src/graphics/shader.cpp $(INCLUDE_FLAGS) -o tmp/shader.o
	$(CC) -c src/graphics/sprite.cpp $(INCLUDE_FLAGS) -o tmp/sprite.o
	$(CC) -c src/input/keyboard.cpp $(INCLUDE_FLAGS) -o tmp/keyboard.o
	$(CC) -c src/core/window.cpp $(INCLUDE_FLAGS) -o tmp/window.o
	$(CC) -c src/stub.cpp $(INCLUDE_FLAGS) -o tmp/stub.o
