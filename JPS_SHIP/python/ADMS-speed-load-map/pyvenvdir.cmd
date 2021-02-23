:;( #
  :;  rm python-3.6.8.exe  #
  :;  rm Pipfile.lock  #
  :;  mkdir pyvenv  #
  :;  mkdir pyvenv/PYVENV_JPS_SHIP #
  :;  mv requirements.txt pyvenv/PYVENV_JPS_SHIP #
  :;  mv pyvenv.cmd pyvenv/PYVENV_JPS_SHIP #
  :;  rm pyvenvdir.cmd #
:; );<<'Executing on Unix-like OS'
(
       python-3.6.8.exe /quiet InstallAllUsers=0 Include_launcher=0 Include_test=0 SimpleInstall=1 
       del python-3.6.8.exe
       del requirements.txt
       mkdir pyvenv
       cd pyvenv
       mkdir PYVENV_JPS_SHIP
       cd ..
       move Pipfile.lock pyvenv/PYVENV_JPS_SHIP 
       move pyvenv.cmd pyvenv/PYVENV_JPS_SHIP
       (goto) 2>nul & del "%~f0"
) & rem ^
 Executing on Windows.