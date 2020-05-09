PLATFORM = linux

INCLUDE_FLAGS = -Isrc/external/glad/include -I/home/arebel/development/tools/glfw/include

LINKER_FLAGS = -L../libs/ -L -lrebel -L/usr/lib/ -L -lglfw3

CSC = g++
CHICKEN_CSC = csc

# Check if really needed
# LINKER_FLAGS += -L -ldl

ifeq ($(PLATFORM),linux)
	CHICKEN_CSC = chicken-csc # If using arch
	LINKER_FLAGS += -L/usr/lib -L -lX11 -L -lpthread 
endif

all:	run

build:	lib

run:
	cd src ; chicken-csc ../scripts/main.scm -o ../output/game $(LINKER_FLAGS) -debug F -c++ -static
	output/game

lib: 	objs
	ar rvs libs/librebel.a tmp/rebel.o tmp/window.o tmp/stub.o tmp/shader.o tmp/sprite.o tmp/glad.o tmp/keyboard.o

objs:
	$(CSC) -c src/external/glad/glad.c $(INCLUDE_FLAGS) -o tmp/glad.o
	$(CSC) -c src/rebel.cpp $(INCLUDE_FLAGS) -o tmp/rebel.o
	$(CSC) -c src/graphics/shader.cpp $(INCLUDE_FLAGS) -o tmp/shader.o
	$(CSC) -c src/graphics/sprite.cpp $(INCLUDE_FLAGS) -o tmp/sprite.o
	$(CSC) -c src/input/keyboard.cpp $(INCLUDE_FLAGS) -o tmp/keyboard.o
	$(CSC) -c src/core/window.cpp $(INCLUDE_FLAGS) -o tmp/window.o
	$(CSC) -c src/stub.cpp $(INCLUDE_FLAGS) -o tmp/stub.o
