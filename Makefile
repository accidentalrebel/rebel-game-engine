PLATFORM = linux
CC = g++
CSC = csc
PREDEFINES =

INCLUDE_FLAGS = -Isrc/external -Isrc/external/glad/include
LINKER_FLAGS = -L../libs/ -L -lrebel 

ifeq ($(PLATFORM),linux)
	CSC = chicken-csc
	LINKER_FLAGS += -L/usr/lib -L -lglfw
else ifeq ($(PLATFORM),macosx)
	LINKER_FLAGS += -L/usr/local/lib -L -lglfw
	PREDEFINES += -D PLATFORM=$(PLATFORM)
else ifeq ($(PLATFORM),windows)
	LINKER_FLAGS += -L/c/tools/msys64/mingw64/lib -L -lglfw3
endif

all:	run

build:	lib

run:
	cd src ; $(CSC) ../scripts/main.scm -o ../output/game $(LINKER_FLAGS) -debug F -c++ -static
	output/game

lib: 	objs
	ar rvs libs/librebel.a tmp/rebel.o tmp/window.o tmp/stub.o tmp/shader.o tmp/camera.o tmp/renderer.o tmp/glad.o tmp/keyboard.o

objs:
	$(CC) -c src/external/glad/src/glad.c $(INCLUDE_FLAGS) -o tmp/glad.o
	$(CC) -c src/rebel.cpp $(INCLUDE_FLAGS) -o tmp/rebel.o
	$(CC) -c src/graphics/shader.cpp $(INCLUDE_FLAGS) -o tmp/shader.o
	$(CC) -c src/graphics/renderer.cpp $(INCLUDE_FLAGS) -o tmp/renderer.o
	$(CC) -c src/graphics/camera.cpp $(INCLUDE_FLAGS) -o tmp/camera.o
	$(CC) -c src/input/keyboard.cpp $(INCLUDE_FLAGS) -o tmp/keyboard.o
	$(CC) -c src/core/window.cpp $(INCLUDE_FLAGS) $(PREDEFINES) -o tmp/window.o
	$(CC) -c src/stub.cpp $(INCLUDE_FLAGS) -o tmp/stub.o

clean:
	rm ./tmp/*.*
	rm ./libs/*.*
