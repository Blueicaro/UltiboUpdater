# UltiboUpdater
Small application to update your ultibo apps (https://ultibo.org/)

 Updater is a small software to automate the process of update
 your Ultibo programs into a raspberry PI

  Copyright (C) 2018 Jorge Turiel jorge.turiel@gmail.com

  This code is release under Creative Common Attribution-ShareAlike 4.0 licence

  ## Release v0.4
* Added more funcions.
* Added Kernelupdate. Now you can update firmware files. Ultibo now accepts send kernel files using telnet. Now you can selected the kernel folder and will be copy to web server, then a update kernel command will be send to your pi with ultibo.
* Added Reboot. Reboot your pi
* Added Type Any command. You can type any command to your ultibo
* Remove update cmdline with the ip server, because the cmdline file has can have more entries, and this entries has to be in one line, so this functions must be rewrite, and for the moment  was removed. In next releases will be back (I hope).

## Release v0.3 
  
Add comandline:
* run. will connect to your Ultibo/Pi and send a update comand. Using        ipwebserve, ip of raspberry etc, of last sesion
* Kernel You can pass the kernel file as parameter.

## Release  v0.2 
First stable realease   
