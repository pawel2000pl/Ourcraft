#!/bin/bash
echo 'Do you want to install requires? [y = yes / anything else = no]'
read CHOICE

if [ "$CHOICE" = "Y" ];
then
    CHOICE='y'
fi
    

if [ "$CHOICE" = "y" ];
then
    echo Installating requires...
    sudo apt install fpc lazarus libopenal-data libopenal-dev libopenal1 libgl-dev freeglut3-dev freeglut3 
    #libopengl-dev libopengl0 libcoin80c
fi
