all: init requires hello compile textures 

all_again: clean all

bin/.initialized:
	date
	chmod u+x installation/*.sh
	chmod u+x source/Preprocesor/preprocesor.sh
	bash -i "installation/init2.sh"
	
init: bin/.initialized	
	
requires: init
	bash -i "./installation/apt_install_requires.sh"
	
compile: init texture_compiler bin/SingleMode

bin/SingleMode:
	bash -i "./source/Preprocesor/preprocesor.sh"	
	lazbuild --build-mode=Release "./source/Single-mode/SingleMode.lpr" 
	
rebuild: init
	rm "bin/SingleMode"
	make compile
	
texture_compiler: bin/TextureCompiler

bin/TextureCompiler:
	fpc -B -Mobjfpc -dUseCThreads -Sc -Sh -Si -ap "./source/TextureCompiler.pas" "-obin/TextureCompiler"
	chmod u+x bin/TextureCompiler
	
textures: init texture_compiler
	bash -i "./installation/compile_textures.sh"	
	
clean:
	chmod u+x installation/cleanup.sh
	bash -i "./installation/cleanup.sh"

clear: clean	
		
hello: init
	bash -i "./installation/hello.sh"	
	
run:
	bin/SingleMode

.PHONY: all init hello compile clean clear requires texture_compiler textures all_again run rebuild
