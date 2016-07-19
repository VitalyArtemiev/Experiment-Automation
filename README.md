# Experiment-Automation
Automated device control for laboratory work

This program intends to help automate experimentation by enabling users to control lab equipment, 
namely function generators, lock-in amplifiers and temperature controllers. 
It also provides means to incorporate new devices into the process, allowing users to create and modify tables 
with device parameters, such as command strings.

This program aims to support several interfaces, such as RS-232 (COM), TelNet and VXI (via Ethernet), and USB. 
Currently only RS-232 and TelNet are fully implemented.

This program was tested with the following equipment: 
Stanford Research Systems function generators DS335, DS345; 
lock-in amplifiers SR830, SR844;
temperature controller PTC10.


This program was created with Lazarus RAD IDE and uses the Lazarus Component Library (lazarus-ide.org).
In order to compile it, one needs at least the Freepascal Compiler 3.0 (freepascal.org) with RTL, FCL (included) and LCL from Lazarus.
The easier method is to just install Lazarus, which comes with the compiler by default. If you want to compile for targets different 
from your machine, though, you'll have to download the required compiler version and put the files accordingly 
in \Lazarus\fpc\<compiler version>\bin\<target>, then switch the target in Lazarus project properties.
The program also has a dependency on ARARAT Synapse library (synapse.ararat.cz).
It can be compiled and run on Windows x86 and x64 machines. It might work on Linux machines with minor modifications.

To workaround several problems in LCL and Synapse, I modified some files in those libraries, such as synaser.pas and editbtn.pas.
You can find them in 'Crutches' folder. Replace them in folders of correlating libraries to achieve intended program behaviour.
The modifications can be seen by searching '<CRUTCH>' in theese files.
