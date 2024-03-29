﻿
# Ourcraft 

Better, bigger and more communist than Minecraft;
(The project is humorou and it is being created for memes)

# Main differences between Ourcraft and Minecraft

Warning: this is project, some (almost all) of this features haven't been implemented yet.

1. [+] There is no height limit
2. [-] Levels instead of other worlds (as Nether or End)
3. [-] More 3D GUI (during the gameplay)
4. [+] It is not executed by Java Virtual Machine (JVM), but it is an executable file, so there is more FPS
5. [+] Three channels for light (red, green, blue)
6. [+] Light with 256 levels (but light-resistance is 16/block)
7. [?] More realistic physics
     
It's not playable yet (but almost). But you can: 

* Fly: WASD, Space (Up) and X (Down)
* Move camera: IJKL 
* Spawn the first entity "MovingBlock": F (a stone block will be placed), or G (a glowstone block will be placed)
* Edit blocks: N (air), M (stone), 0-7 (colored lightstone); only in constant and determined distance

Changelog:

* Added working dynamic light.
* Added (only) directive for using CORBA interfaces. TEnvironmentElementAttribute will be removed in a near future.
* Optimalization in temporary rendering (SingleMode)
* Update to FPC 3.2 (it is NOT compatible with 3.0)
* Update for newer kernels (fastcmem was deactivated)
* Changed algorithm of light: instead of only gray it is red, green and blue (and mixing of them is possible)
* Day-Night cycle for block lighting
* Saving and loading blocks (chunks)
* Added Perlin Noise algorithm for any count of dimenstion
* Added simply terrain generator
* Bug fixes and optimalization
* Added physic engine (it still does not work as it will, but work with a slow velocity)
* Temporary the physic engine is simply, but it works
* Added some documentation


How to compile and run

1. Install required packages: "installation/apt install_requires". It may be necessary to change version of some libs, so then edit the file.
2. Execute "run.sh"
3. An executable file should be in "bin/SingleMode" (other files do nothing for gameplay)

or execute "make all" to install and compile - and then execute "make run" to open the game in single mode.

In case of errors with the compilation, open with the Lazarus "source/Single-mode/SingleMode.lpr" and then click Package -> Open loaded package.
Find "lazopenglcontext", open it and compile. Then click Run -> Build.
And then, you can type "make run" (in the console, in main directory of project) to open the game in single mode.

Screenshots:

![Screenshot](screenshots/Screenshot_20210629_152853.png)
![Screenshot](screenshots/Screenshot_20201230_215039.png)
![Screenshot](screenshots/Screenshot_20201214_225918.png)
![Screenshot](screenshots/Screenshot_20201006_220412.png)
![Screenshot](screenshots/Screenshot_20201006_174602.png)
![Screenshot](screenshots/Screenshot_20200825_215336.png)
![Screenshot](screenshots/Screenshot_20200825_215243.png)
![Screenshot](screenshots/Screenshot_20200825_215226.png)
![Screenshot](screenshots/Screenshot_20200528_155109.png)
![Screenshot](screenshots/Screenshot_20200525_000632.png)
![Screenshot](screenshots/Screenshot_20200524_133030.png)
