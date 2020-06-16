
PLATFORM = linux
CC = gcc
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
	cd src ; $(CSC) -cxx $(CC) ../scripts/main.scm -o ../output/game $(LINKER_FLAGS) -I../src/ -debug F -static
	output/game

lib: 	objs
	ar rvs libs/librebel.a tmp/rebel.o tmp/vec3.o tmp/window.o tmp/shader.o tmp/camera.o tmp/renderer.o tmp/glad.o tmp/keyboard.o tmp/mouse.o tmp/light.o tmp/material.o tmp/mesh.o tmp/model.o tmp/ffi-test.o

objs:
	$(CC) -c src/ffi-test.c $(INCLUDE_FLAGS) $(PREDEFINES) -o tmp/ffi-test.o
	$(CC) -c src/external/glad/src/glad.c $(INCLUDE_FLAGS) -o tmp/glad.o
	$(CC) -c src/data/vec3.c $(INCLUDE_FLAGS) -o tmp/vec3.o
	$(CC) -c src/rebel.c $(INCLUDE_FLAGS) -o tmp/rebel.o
	$(CC) -c src/graphics/model.c $(INCLUDE_FLAGS) -o tmp/model.o	
	$(CC) -c src/graphics/mesh.c $(INCLUDE_FLAGS) -o tmp/mesh.o
	$(CC) -c src/graphics/shader.c $(INCLUDE_FLAGS) -o tmp/shader.o
	$(CC) -c src/graphics/renderer.c $(INCLUDE_FLAGS) -o tmp/renderer.o
	$(CC) -c src/graphics/camera.c $(INCLUDE_FLAGS) -o tmp/camera.o
	$(CC) -c src/graphics/material.c $(INCLUDE_FLAGS) -o tmp/material.o
	$(CC) -c src/graphics/lighting/light.c $(INCLUDE_FLAGS) -o tmp/light.o
	$(CC) -c src/input/keyboard.c $(INCLUDE_FLAGS) -o tmp/keyboard.o
	$(CC) -c src/input/mouse.c $(INCLUDE_FLAGS) -o tmp/mouse.o
	$(CC) -c src/core/window.c $(INCLUDE_FLAGS) $(PREDEFINES) -o tmp/window.o

clean:
	rm ./tmp/*.*
	rm ./libs/*.*
