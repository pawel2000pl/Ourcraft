#!/bin/bash
ls Textures/*.png | ./bin/TextureCompiler | tee "compiled-resources/RenderedTextures.txt" || exit
mv "Rendered.png" "compiled-resources/RenderedTextures.png"
echo There are `cat "compiled-resources/RenderedTextures.txt" | wc -l` textures
