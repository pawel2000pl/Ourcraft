all:  init requires hello compile textures 

init:
	date
	chmod u+x installation/*.sh
	chmod u+x source/Preprocesor/preprocesor.sh
	mkdir "bin" 2> "/dev/null"
	mkdir "compiled-resources" 2> "/dev/null"
	
requires: init
	bash -i "./installation/apt_install_requires.sh"
	
compile: init texture_compiler
	bash -i "./source/Preprocesor/preprocesor.sh"	
	lazbuild "./source/Single-mode/SingleMode.lpr" 
	mv "./source/Single-mode/SingleMode" "./bin/"
	rm -rf "./source/Single-mode/lib"
	chmod u+x "bin/OurCraft"
	
texture_compiler: bin/TextureCompiler
	fpc -B -Mobjfpc -dUseCThreads -Sc -Sh -Si -ap "./source/TextureCompiler.pas" "-obin/TextureCompiler"
	chmod u+x "bin/TextureCompiler"
	
textures: init texture_compiler
	bash -i "./installation/compile_textures.sh"	
	
clean: init
	bash -i "./installation/cleanup.sh"

clear: clean	
		
hello: init
	bash -i "./installation/hello.sh"	

.PHONY: all init hello compile clean clear requires texture_compiler textures
