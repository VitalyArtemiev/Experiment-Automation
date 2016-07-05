# Experiment-Automation
Automated device control for laboratory work

This program intends to help automate experimentation by providing means to control lab equipment, 
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
In order to compile it, one needs at least the Freepascal Compiler 3.0 (freepascal.org) with RTL, FCL and LCL from Lazarus.  
It also has a dependency on ARARAT Synapse library (synapse.ararat.cz).
This program can be compiled and run on Windows x86 and x64 machines. It might work on Linux machines with minor modifications.

