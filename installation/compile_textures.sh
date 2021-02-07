#!/bin/bash
ls Textures/*.png | ./bin/TextureCompiler > "compiled-resources/RenderedTextures.txt" || exit
mv "Rendered.png" "compiled-resources/RenderedTextures.png"
echo There are `cat "compiled-resources/RenderedTextures.txt" | tail -n +1 | wc -l` textures
