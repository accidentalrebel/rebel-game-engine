
# If this Makefile seems a little weird. Don't worry I am aware that I don't know how to properly make a Makefile.
# I'll work on improving this in the future.

# SPECIFY $(MAIN_FILE) and $(OUTPUT_FILE)

PLATFORM = linux
CC = gcc
CSC = csc
PREDEFINES =

INCLUDE_FLAGS = -Isrc/external -Isrc/external/glad/include -Isrc/external/glfw/include -I/usr/include 
LINKER_FLAGS = -L../libs/ -L -lrebel -L/usr/lib

ifeq ($(PLATFORM),linux)
	CSC = chicken-csc
	LINKER_FLAGS += -L/usr/lib -L -lglfw -L -lassimp
else ifeq ($(PLATFORM),macosx)
	LINKER_FLAGS += -L/usr/local/lib -L -lglfw -L -lassimp
	PREDEFINES += -D PLATFORM=$(PLATFORM)
else ifeq ($(PLATFORM),windows)
	LINKER_FLAGS += -L/c/msys64/mingw64/lib -L -lglfw3 -L -lassimp
endif

all:	run

build:	lib

debug: 	DEBUG = -g
debug: 	DEBUG_CHICKEN = -d3 -debug F
debug: 	lib
debug: 	all

run:
	cd src ; $(CSC) -cxx $(CC) $(MAIN_FILE) -o $(OUTPUT_FILE) $(LINKER_FLAGS) -I../src/ $(DEBUG_CHICKEN) -static
	$(OUTPUT_FILE)

lib: 	objs
	ar rvs libs/librebel.a tmp/rebel.o tmp/window.o tmp/shader.o tmp/camera.o tmp/renderer.o tmp/glad.o tmp/keyboard.o tmp/mouse.o tmp/light.o tmp/material.o tmp/mesh.o tmp/model.o

objs:
	$(CC) $(DEBUG) -c src/external/glad/src/glad.c $(INCLUDE_FLAGS) -o tmp/glad.o
	$(CC) $(DEBUG) -c src/rebel.c $(INCLUDE_FLAGS) -o tmp/rebel.o
	$(CC) $(DEBUG) -c src/graphics/model.c $(INCLUDE_FLAGS) -o tmp/model.o	
	$(CC) $(DEBUG) -c src/graphics/mesh.c $(INCLUDE_FLAGS) -o tmp/mesh.o
	$(CC) $(DEBUG) -c src/graphics/shader.c $(INCLUDE_FLAGS) -o tmp/shader.o
	$(CC) $(DEBUG) -c src/graphics/renderer.c $(INCLUDE_FLAGS) -o tmp/renderer.o
	$(CC) $(DEBUG) -c src/graphics/camera.c $(INCLUDE_FLAGS) -o tmp/camera.o
	$(CC) $(DEBUG) -c src/graphics/material.c $(INCLUDE_FLAGS) -o tmp/material.o
	$(CC) $(DEBUG) -c src/graphics/lighting/light.c $(INCLUDE_FLAGS) -o tmp/light.o
	$(CC) $(DEBUG) -c src/input/keyboard.c $(INCLUDE_FLAGS) -o tmp/keyboard.o
	$(CC) $(DEBUG) -c src/input/mouse.c $(INCLUDE_FLAGS) -o tmp/mouse.o
	$(CC) $(DEBUG) -c src/core/window.c $(INCLUDE_FLAGS) $(PREDEFINES) -o tmp/window.o

clean:
	rm ./tmp/*.*
	rm ./libs/*.*
