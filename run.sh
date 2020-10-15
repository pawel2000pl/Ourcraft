#!/bin/bash
echo Starting…
date
mkdir "bin" 2> "/dev/null"
mkdir "compiled-resources" 2> "/dev/null"
echo Compiler info:
fpc -iD -iV -iSO -iSP -iTO -iTP 

echo The project has `./installation/ShowAllLines.sh | wc -l` lines
echo The author of the project is Paweł Bielecki
sleep 1.5s

echo Running preprocesor…
chmod u+x "source/preprocesor/preprocesor.sh"
./source/preprocesor/preprocesor.sh

echo Compiling texture compiler…
fpc -B -Mobjfpc -dUseCThreads -Sc -Sh -Si -ap "./source/TextureCompiler.pas" "-obin/TextureCompiler" || exit
chmod u+x bin/TextureCompiler
echo Compiling textures…
ls Textures/*.png | ./bin/TextureCompiler | tee "compiled-resources/RenderedTextures.txt" || exit
mv "Rendered.png" "compiled-resources/RenderedTextures.png"
echo There are `cat "compiled-resources/RenderedTextures.txt" | wc -l` textures

echo Compiling…
#fpc -B -Mobjfpc -dUseCThreads -Sc -Sh -Si -ap "source/OurCraft.pas" $(echo -I$(find /usr/share/lazarus/ -name openglcontext.pas | grep -P -o "\/(([\w\.]*)\/)*" | head -c -2) ) "-Isource/Environment/" "-obin/OurCraft" || exit
lazbuild "./source/Single-mode/SingleMode.lpr" || exit
mv "./source/Single-mode/SingleMode" "./bin/"
rm -rf "./source/Single-mode/lib"
find . -name "*.o" -exec rm {} \;
find . -name "*.ppu" -exec rm {} \;
chmod u+x "bin/OurCraft"
echo Compiled
date
