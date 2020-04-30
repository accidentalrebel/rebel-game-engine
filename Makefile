OBJS = main.cpp src/rebel.cpp src/core/window.cpp src/external/glad.c 

OBJ_NAME = main

CC = g++

COMPILER_FLAGS = 

LINKER_FLAGS = -lGL -lX11 -lpthread -ldl -lglfw3

INCLUDE_FLAGS = -I/home/arebel/development/tools/glad/include -I/home/arebel/development/tools/glfw/include

all: $(OBJS)
	$(CC) $(OBJS) $(COMPILER_FLAGS) $(INCLUDE_FLAGS) $(LINKER_FLAGS) -o output/$(OBJ_NAME)
	output/$(OBJ_NAME)
