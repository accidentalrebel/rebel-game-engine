OBJS = main.cpp src/rebel.cpp src/external/glad.c

OBJ_NAME = main

CC = g++

COMPILER_FLAGS = 

LINKER_FLAGS = -lGL -ldl

INCLUDE_FLAGS = -I/home/arebel/development/tools/glad/include

all: $(OBJS)
	$(CC) $(OBJS) $(COMPILER_FLAGS) $(INCLUDE_FLAGS) $(LINKER_FLAGS) -o output/$(OBJ_NAME)
	output/$(OBJ_NAME)
