#!/bin/bash
MAIN=$PWD/$1/main.scm
OUTPUT=$PWD/$1/output.rbl
make -C ../../ run MAIN_FILE=$MAIN OUTPUT_FILE=$OUTPUT
$OUTPUT
