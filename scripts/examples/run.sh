#!/bin/bash
MAIN=$PWD/$1/main.scm
OUTPUT=$PWD/$1/game
cd ../../
make run MAIN_FILE=$MAIN OUTPUT_FILE=$OUTPUT
