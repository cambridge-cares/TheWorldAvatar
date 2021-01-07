:;( #
  :;  rm python-3.6.8.exe  #
  :;  rm Pipfile*  #
  :;  pip install pipenv  #
  :;  pipenv install --python 3.6 -r requirements.txt  #
:; );<<'Executing on Unix-like OS'
(
       python-3.6.8.exe /quiet InstallAllUsers=0 Include_launcher=0 Include_test=0 SimpleInstall=1 
       python -m pip install pipenv
       python -m pipenv install --python 3.6 --ignore-pipfile
) & rem ^
 Executing on Windows.