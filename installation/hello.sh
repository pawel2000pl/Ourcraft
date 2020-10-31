#!/bin/bash

echo Compiler info:
fpc -iD -iV -iSO -iSP -iTO -iTP 

echo The project has `./installation/ShowAllLines.sh | wc -l` lines
echo The author of the project is Paweł Bielecki
	
