@echo off
echo starting ForageMod sensitivity analysis...

set _JAVA_OPTIONS=-Dcom.sun.media.jai.disableMediaLib=true -Xmx131072m -Dfile.encoding=UTF-8

call "C:\Program Files\NetLogo 7.0.3\NetLogo_Console.exe" ^
     --headless ^
     --model "ForageModv03.nlogox" ^
     --experiment "sensitivity_analysis_parameters" ^
     --update-plots ^
     --threads 16

echo sensitivity analysis finished
pause